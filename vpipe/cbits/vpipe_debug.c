#include <stdatomic.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <vulkan/vulkan.h>

#define VPIPE_DEBUG_ID_CAPACITY 128U
#define VPIPE_DEBUG_MESSAGE_CAPACITY 1024U

struct vpipe_debug_entry {
  uint32_t severity;
  uint32_t type;
  char message_id[VPIPE_DEBUG_ID_CAPACITY];
  char message[VPIPE_DEBUG_MESSAGE_CAPACITY];
};

struct vpipe_debug_sink {
  atomic_uint lock;
  size_t capacity;
  size_t read_index;
  size_t count;
  uint64_t dropped;
  struct vpipe_debug_entry entries[];
};

static void vpipe_debug_lock(struct vpipe_debug_sink *sink) {
  unsigned int expected = 0U;
  while (!atomic_compare_exchange_weak_explicit(
      &sink->lock, &expected, 1U, memory_order_acquire,
      memory_order_relaxed)) {
    expected = 0U;
  }
}

static void vpipe_debug_unlock(struct vpipe_debug_sink *sink) {
  atomic_store_explicit(&sink->lock, 0U, memory_order_release);
}

static void vpipe_debug_copy(char *destination, size_t capacity,
                             const char *source) {
  if (source == NULL) {
    destination[0] = '\0';
    return;
  }

  size_t length = 0U;
  while (length + 1U < capacity && source[length] != '\0') {
    length += 1U;
  }
  memcpy(destination, source, length);
  destination[length] = '\0';
}

static void vpipe_debug_push(struct vpipe_debug_sink *sink, uint32_t severity,
                             uint32_t type, const char *message_id,
                             const char *message) {
  if (sink == NULL) {
    return;
  }

  vpipe_debug_lock(sink);
  if (sink->count == sink->capacity) {
    if (sink->dropped != UINT64_MAX) {
      sink->dropped += 1U;
    }
    vpipe_debug_unlock(sink);
    return;
  }

  size_t write_index = (sink->read_index + sink->count) % sink->capacity;
  struct vpipe_debug_entry *entry = &sink->entries[write_index];
  entry->severity = severity;
  entry->type = type;
  vpipe_debug_copy(entry->message_id, VPIPE_DEBUG_ID_CAPACITY, message_id);
  vpipe_debug_copy(entry->message, VPIPE_DEBUG_MESSAGE_CAPACITY, message);
  sink->count += 1U;
  vpipe_debug_unlock(sink);
}

void *vpipe_debug_sink_create(size_t capacity) {
  if (capacity == 0U ||
      capacity > (SIZE_MAX - sizeof(struct vpipe_debug_sink)) /
                     sizeof(struct vpipe_debug_entry)) {
    return NULL;
  }

  struct vpipe_debug_sink *sink =
      calloc(1U, sizeof(struct vpipe_debug_sink) +
                     capacity * sizeof(struct vpipe_debug_entry));
  if (sink == NULL) {
    return NULL;
  }

  atomic_init(&sink->lock, 0U);
  sink->capacity = capacity;
  return sink;
}

int vpipe_debug_sink_pop(void *user_data, uint32_t *severity, uint32_t *type,
                         char *message_id, size_t message_id_capacity,
                         char *message, size_t message_capacity) {
  struct vpipe_debug_sink *sink = user_data;
  if (sink == NULL || severity == NULL || type == NULL || message_id == NULL ||
      message_id_capacity == 0U || message == NULL || message_capacity == 0U) {
    return 0;
  }

  vpipe_debug_lock(sink);
  if (sink->count == 0U) {
    vpipe_debug_unlock(sink);
    return 0;
  }

  const struct vpipe_debug_entry *entry = &sink->entries[sink->read_index];
  *severity = entry->severity;
  *type = entry->type;
  vpipe_debug_copy(message_id, message_id_capacity, entry->message_id);
  vpipe_debug_copy(message, message_capacity, entry->message);
  sink->read_index = (sink->read_index + 1U) % sink->capacity;
  sink->count -= 1U;
  vpipe_debug_unlock(sink);
  return 1;
}

uint64_t vpipe_debug_sink_dropped(void *user_data) {
  struct vpipe_debug_sink *sink = user_data;
  if (sink == NULL) {
    return 0U;
  }

  vpipe_debug_lock(sink);
  uint64_t dropped = sink->dropped;
  vpipe_debug_unlock(sink);
  return dropped;
}

void vpipe_debug_sink_free(void *user_data) { free(user_data); }

VKAPI_ATTR VkBool32 VKAPI_CALL vpipe_debug_callback(
    VkDebugUtilsMessageSeverityFlagBitsEXT severity,
    VkDebugUtilsMessageTypeFlagsEXT type,
    const VkDebugUtilsMessengerCallbackDataEXT *callback_data,
    void *user_data) {
  const char *message_id = callback_data == NULL
                               ? NULL
                               : callback_data->pMessageIdName;
  const char *message = callback_data == NULL ? NULL : callback_data->pMessage;
  vpipe_debug_push(user_data, (uint32_t)severity, (uint32_t)type, message_id,
                   message);
  return VK_FALSE;
}

void vpipe_debug_sink_test_callback(void *user_data, uint32_t severity,
                                    uint32_t type, const char *message_id,
                                    const char *message) {
  VkDebugUtilsMessengerCallbackDataEXT callback_data = {
      .sType = VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT,
      .pMessageIdName = message_id,
      .pMessage = message,
  };
  (void)vpipe_debug_callback(
      (VkDebugUtilsMessageSeverityFlagBitsEXT)severity,
      (VkDebugUtilsMessageTypeFlagsEXT)type, &callback_data, user_data);
}

VkResult vpipe_create_device(PFN_vkCreateDevice create_device,
                             VkPhysicalDevice physical_device,
                             const VkDeviceCreateInfo *create_info,
                             VkDevice *device) {
  if (create_device == NULL || physical_device == VK_NULL_HANDLE ||
      create_info == NULL || device == NULL) {
    return VK_ERROR_INITIALIZATION_FAILED;
  }

  VkDeviceCreateInfo fixed_create_info = *create_info;
  fixed_create_info.enabledLayerCount = 0U;
  fixed_create_info.ppEnabledLayerNames = NULL;
  return create_device(physical_device, &fixed_create_info, NULL, device);
}
