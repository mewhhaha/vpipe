#!/usr/bin/env python3

from __future__ import annotations

import pathlib
import re
import sys
import unicodedata
import urllib.error
import urllib.parse
import urllib.request


ROOT = pathlib.Path(__file__).resolve().parents[2]
SKIPPED_DIRECTORIES = {".agents", ".codex", ".git", "artifacts", "dist-newstyle"}
HASKELL_SOURCE_ROOTS = (ROOT / "vpipe/src", ROOT / "vpipe-glfw/src")
INLINE_LINK = re.compile(r"!?\[[^]]*]\((?P<target><[^>]+>|[^\s)]+)(?:\s+[^)]*)?\)")
HADDOCK_LINK = re.compile(r"<(?P<target>https?://[^>\s]+)>", re.IGNORECASE)
HEADING = re.compile(r"^#{1,6}\s+(?P<title>.+?)\s*#*\s*$")
FENCE = re.compile(r"^\s*(```|~~~)")
EXTERNAL_TIMEOUT_SECONDS = 10
EXTERNAL_ATTEMPTS = 3


def markdown_files() -> list[pathlib.Path]:
    return sorted(
        path
        for path in ROOT.rglob("*.md")
        if not SKIPPED_DIRECTORIES.intersection(path.relative_to(ROOT).parts)
    )


def haskell_source_files() -> list[pathlib.Path]:
    return sorted(
        source_file
        for source_root in HASKELL_SOURCE_ROOTS
        if source_root.is_dir()
        for source_file in source_root.rglob("*.hs")
    )


def heading_slug(title: str) -> str:
    lowered = title.casefold().strip()
    without_markup = re.sub(r"[`*_~]", "", lowered)
    without_punctuation = "".join(
        character
        for character in without_markup
        if not unicodedata.category(character).startswith("P") or character == "-"
    )
    return re.sub(r"\s", "-", without_punctuation)


def markdown_anchors(path: pathlib.Path) -> set[str]:
    anchors: set[str] = set()
    occurrences: dict[str, int] = {}
    fence_marker: str | None = None
    for line in path.read_text(encoding="utf-8").splitlines():
        fence = FENCE.match(line)
        if fence:
            marker = fence.group(1)
            if fence_marker is None:
                fence_marker = marker
            elif marker == fence_marker:
                fence_marker = None
            continue
        if fence_marker is not None:
            continue
        heading = HEADING.match(line)
        if heading is None:
            continue
        base = heading_slug(heading.group("title"))
        duplicate_index = occurrences.get(base, 0)
        occurrences[base] = duplicate_index + 1
        anchors.add(base if duplicate_index == 0 else f"{base}-{duplicate_index}")
    return anchors


def local_target(link: str) -> tuple[str, str]:
    unwrapped = link[1:-1] if link.startswith("<") and link.endswith(">") else link
    parsed = urllib.parse.urlsplit(unwrapped)
    if parsed.scheme or parsed.netloc:
        return "", ""
    return urllib.parse.unquote(parsed.path), urllib.parse.unquote(parsed.fragment)


def external_target(link: str) -> str | None:
    unwrapped = link[1:-1] if link.startswith("<") and link.endswith(">") else link
    parsed = urllib.parse.urlsplit(unwrapped)
    if parsed.scheme.casefold() not in {"http", "https"} or not parsed.netloc:
        return None
    return urllib.parse.urlunsplit((parsed.scheme, parsed.netloc, parsed.path, parsed.query, ""))


def check_document(path: pathlib.Path) -> list[str]:
    failures: list[str] = []
    source = path.read_text(encoding="utf-8")
    for match in INLINE_LINK.finditer(source):
        linked_path, fragment = local_target(match.group("target"))
        if not linked_path and not fragment:
            continue
        destination = (path.parent / linked_path).resolve() if linked_path else path.resolve()
        try:
            destination.relative_to(ROOT)
        except ValueError:
            failures.append(f"{path.relative_to(ROOT)}: link escapes repository: {match.group('target')}")
            continue
        if not destination.exists():
            failures.append(f"{path.relative_to(ROOT)}: missing link target: {match.group('target')}")
            continue
        if fragment and destination.suffix.casefold() == ".md":
            if fragment not in markdown_anchors(destination):
                failures.append(
                    f"{path.relative_to(ROOT)}: missing Markdown anchor {fragment!r} in {destination.relative_to(ROOT)}"
                )
    return failures


def check_external_link(link: str) -> str | None:
    request = urllib.request.Request(link, headers={"User-Agent": "vpipe-link-check/1.0"})
    failure = "request did not complete"
    for attempt in range(1, EXTERNAL_ATTEMPTS + 1):
        try:
            with urllib.request.urlopen(request, timeout=EXTERNAL_TIMEOUT_SECONDS) as response:
                if 200 <= response.status < 400:
                    return None
                failure = f"received HTTP {response.status}"
        except urllib.error.HTTPError as error:
            failure = f"received HTTP {error.code}: {error.reason}"
            if 400 <= error.code < 500 and error.code not in {408, 429}:
                return failure
        except (OSError, ValueError) as error:
            failure = f"{type(error).__name__}: {error}"
        if attempt == EXTERNAL_ATTEMPTS:
            return (
                f"failed after {attempt} attempts with {EXTERNAL_TIMEOUT_SECONDS}s timeout: {failure}"
            )
    raise AssertionError("external link check did not return")


def external_links(
    documents: list[pathlib.Path], source_documents: list[pathlib.Path] | None = None
) -> dict[str, list[str]]:
    links: dict[str, list[str]] = {}
    for document in documents:
        source = document.read_text(encoding="utf-8")
        for match in INLINE_LINK.finditer(source):
            link = external_target(match.group("target"))
            if link is not None:
                links.setdefault(link, []).append(str(document.relative_to(ROOT)))
    for source_document in source_documents or []:
        source = source_document.read_text(encoding="utf-8")
        for line_number, line in enumerate(source.splitlines(), start=1):
            for match in HADDOCK_LINK.finditer(line):
                link = external_target(match.group("target"))
                if link is not None:
                    source_name = f"{source_document.relative_to(ROOT)}:{line_number} (Haddock)"
                    links.setdefault(link, []).append(source_name)
    return links


def main() -> int:
    check_external = "--check-external" in sys.argv[1:]
    unexpected_arguments = set(sys.argv[1:]) - {"--check-external"}
    if unexpected_arguments:
        print("usage: check-links.py [--check-external]", file=sys.stderr)
        return 2

    documents = markdown_files()
    failures = [failure for document in documents for failure in check_document(document)]
    checked_external_links = 0
    if check_external:
        links = external_links(documents, haskell_source_files())
        checked_external_links = len(links)
        for link, sources in links.items():
            failure = check_external_link(link)
            if failure is not None:
                source_names = ", ".join(sources)
                failures.append(f"{source_names}: external link {link}: {failure}")
    if failures:
        print("\n".join(failures), file=sys.stderr)
        return 1
    print(f"Checked {len(documents)} Markdown files; all repository-local links resolve.")
    if check_external:
        print(f"Checked {checked_external_links} external HTTP(S) links.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
