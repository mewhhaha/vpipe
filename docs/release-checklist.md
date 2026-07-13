# 0.1 release checklist

The `vpipe` and `vpipe-glfw` packages are one release train.  A release is not
ready if only one archive passes or if their versions differ.

## Prepare

- Confirm both package versions and the examples version are `0.1.0.0`, with
  PVP bounds `>= 0.1 && < 0.2` for internal dependencies.
- Finish the matching `CHANGELOG.md` entries and replace “unreleased” with the
  intended date only in the release commit.
- Run `scripts/release/verify-candidate.sh` from a clean checkout.  Keep its
  sdists, Haddock archives, isolated-build proof, validation log, and shader
  dumps together.
- Inspect every file in each sdist, confirm both MIT license files, and verify
  the GPipe/fir acknowledgements.  vpipe contains inspired ideas but no copied
  source from either project.
- Run the screenshot checker on the pinned lavapipe image and inspect the
  rendered diff rather than accepting a regenerated golden blindly.
- Keep CI's [Canonical snapshot](https://documentation.ubuntu.com/server/how-to/software/snapshot-service/)
  and `mesa-vulkan-drivers` version paired.  The 0.1 goldens use snapshot
  `20260701T120000Z` and Mesa
  `25.2.8-0ubuntu0.24.04.2`; refresh both deliberately before the snapshot's
  retention window ends, regenerate every golden, and review the visual diff.

## Candidate only

Run the **Hackage release candidate** workflow with `upload_candidate=true`.
The `hackage-candidate` GitHub environment must require a maintainer approval
and provide a candidate-only `HACKAGE_TOKEN`.  The workflow deliberately calls
`cabal upload` without `--publish`; Cabal therefore uploads package candidates,
not irreversible index releases.

Check both candidate pages and install them together in a fresh environment.
Do not publish either package while the other candidate is missing or broken.

## Review window

Share the candidate URLs, tutorial, platform matrix, and known 0.1 limitations
with all of these audiences:

- Haskell Discourse;
- r/haskell;
- GPipe users or maintainers who can assess the migration notes;
- at least one Haskeller who has not used Vulkan, asked to time the first
  triangle tutorial from a stock Linux installation.

Record links to the review threads, the novice walkthrough time, and every
release-blocking issue in the release PR.  Require explicit sign-off that the
API docs, triangle, compute example, and installation instructions match the
candidate artifacts.

## External evidence gates

These gates remain unchecked until the release PR contains the linked evidence.
They are not satisfied by Linux CI or local lavapipe results.

- [ ] Run the windowed examples on a physical Wayland desktop and attach the
  validation log and outcome.
- [ ] Run the supported windowed examples on physical Windows hardware and
  attach the validation log and outcome.
- [ ] Upload and install both Hackage candidates together; link both candidate
  pages and the clean-install result.
- [ ] Record review from the planned external audiences, including GPipe users
  or maintainers, and resolve or explicitly defer each blocking issue.
- [ ] Record a novice's stock-Linux first-triangle walkthrough and its elapsed
  time; it must meet the under-30-minute acceptance criterion.

## Publish

Publishing is a separate, manual maintainer action after review fixes have
produced and re-verified new candidates.  Hackage releases cannot be deleted,
so the candidate workflow never passes `--publish`.  Publish `vpipe` first and
`vpipe-glfw` immediately afterward, upload the matching documentation with
`--documentation --publish`, tag the exact release commit, and then verify a
clean index install before announcing the release.
