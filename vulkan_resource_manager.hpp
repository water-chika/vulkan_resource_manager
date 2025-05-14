#pragma once

#include <unordered_map>

#include <vulkan/vulkan.hpp>
#include <vulkan/vulkan_hash.hpp>
#undef max

namespace vulkan_resource_manage {
class command_buffer_manager {
public:
	command_buffer_manager(vk::Device device, vk::CommandPool pool)
		:
		m_device{ device },
		m_pool{pool}
	{}

	auto allocate() {
		return m_device.allocateCommandBuffers(
			vk::CommandBufferAllocateInfo{}.setCommandPool(m_pool).setCommandBufferCount(1)
		)[0];
	}
	void free(vk::CommandBuffer buffer) {
		m_device.freeCommandBuffers(m_pool, buffer);
	}
private:
	vk::Device m_device;
	vk::CommandPool m_pool;
};

static inline std::vector<vk::DescriptorPoolSize> to_pool_sizes(std::unordered_map<vk::DescriptorType, uint32_t> type_counts) {
	std::vector<vk::DescriptorPoolSize> pool_sizes(type_counts.size());
	std::transform(type_counts.begin(), type_counts.end(), pool_sizes.begin(),
		[](auto type_count) {
			auto [type, count] = type_count;
			return vk::DescriptorPoolSize{}.setType(type).setDescriptorCount(count);
		});
	return pool_sizes;
}

class descriptor_set_manager {
public:
	descriptor_set_manager(vk::Device device, vk::DescriptorSetLayout layout, std::vector<vk::DescriptorPoolSize> pool_sizes)
		:
		m_device{ device },
		m_layout{ layout },
		m_pool_sizes{ pool_sizes },
		m_pools{}
	{
		increase_pool();
	}
	descriptor_set_manager(vk::Device device, vk::DescriptorSetLayout layout, std::unordered_map<vk::DescriptorType, uint32_t> type_counts)
		: descriptor_set_manager{ device, layout, to_pool_sizes(type_counts) }
	{}

	~descriptor_set_manager() {
		for (auto pool : m_pools) {
			try {
				m_device.destroyDescriptorPool(pool);
			}
			catch (...)
			{
			}
		}
		m_pools.clear();
	}

	descriptor_set_manager(const descriptor_set_manager&) = delete;
	descriptor_set_manager(descriptor_set_manager&&) = default;
	descriptor_set_manager& operator=(const descriptor_set_manager&) = delete;
	descriptor_set_manager& operator=(descriptor_set_manager&&) = default;

	auto allocate_descriptor_set_without_increase_pool() {
		auto pool = m_pools.back();
		auto set = m_device.allocateDescriptorSets(
			vk::DescriptorSetAllocateInfo{}.setDescriptorPool(pool).setSetLayouts(m_layout).setDescriptorSetCount(1)
		)[0];
		return set;
	}

	void increase_pool() {
		auto pool = m_device.createDescriptorPool(
			vk::DescriptorPoolCreateInfo{}.setPoolSizes(m_pool_sizes).setMaxSets(256)
		);
		m_pools.emplace_back(pool);
	}

	auto allocate() {
		vk::DescriptorSet set{};
		if (!m_sets.empty()) {
			set = m_sets.back();
			m_sets.pop_back();
		}
		else {
			try {
				set = allocate_descriptor_set_without_increase_pool();
			}
			catch (vk::OutOfPoolMemoryError& error) {
				increase_pool();
				set = allocate_descriptor_set_without_increase_pool();
			}
		}
		return set;
	}

	void free(vk::DescriptorSet set) {
		m_sets.emplace_back(set);
	}

private:
	vk::Device m_device;
	vk::DescriptorSetLayout m_layout;
	std::vector<vk::DescriptorPool> m_pools;
	std::vector<vk::DescriptorPoolSize> m_pool_sizes;

	std::vector<vk::DescriptorSet> m_sets;
};
class fence_manager {
public:
	fence_manager(vk::Device device)
		:
		m_device{ device }
	{}

	~fence_manager() {
		for (auto fence : m_fences) {
			try {
				m_device.destroyFence(fence);
			}
			catch (...) {

			}
		}
		m_fences.clear();
	}

	fence_manager(const fence_manager&) = delete;
	fence_manager(fence_manager&&) = default;
	fence_manager& operator=(const fence_manager&) = delete;
	fence_manager& operator=(fence_manager&&) = default;

	auto create_fence() {
		return m_device.createFence(vk::FenceCreateInfo{});
	}
	auto destroy_fence(vk::Fence fence) {
		return m_device.destroyFence(fence);
	}

	auto allocate_fence() {
		if (m_fences.empty()) {
			m_fences.emplace_back(create_fence());
		}
		auto fence = m_fences.back();
		m_fences.pop_back();
		assert(m_device.getFenceStatus(fence) == vk::Result::eNotReady);
		return fence;
	}
	auto free_fence(vk::Fence fence) {
		m_device.resetFences(fence);
		m_fences.emplace_back(fence);
	}

private:
	vk::Device m_device;
	std::vector<vk::Fence> m_fences;
};

class semaphore_manager {
public:
	semaphore_manager(vk::Device device)
		:
		m_device{ device }
	{
	}

	auto create_semaphore() {
		return m_device.createSemaphore(vk::SemaphoreCreateInfo{});
	}
	void destroy_semaphore(vk::Semaphore semaphore) {
		m_device.destroySemaphore(semaphore);
	}

	auto allocate_semaphore() {
		if (m_semaphores.empty()) {
			m_semaphores.emplace_back(create_semaphore());
		}
		auto semaphore = m_semaphores.back();
		m_semaphores.pop_back();
		assert(0 == m_device.getSemaphoreCounterValue(semaphore));
		return semaphore;
	}
	void free_semaphore(vk::Semaphore semaphore) {
		m_device.signalSemaphore(vk::SemaphoreSignalInfo{}.setSemaphore(semaphore).setValue(0));
		m_semaphores.emplace_back(semaphore);
	}

private:
	vk::Device m_device;
	std::vector<vk::Semaphore> m_semaphores;
};

template<typename Resource, typename ResourceAllocator>
class resource_manager {
public:
	resource_manager(vk::Device device,
		ResourceAllocator resource_allocator,
		fence_manager fence_m)
		:
		m_device{ device },
		m_resource_allocator{ std::move(resource_allocator) },
		m_fence_manager{ std::move(fence_m) }
	{
	}

	~resource_manager() {
		if (!m_fence_resources.empty()) {
			auto fences = std::vector<vk::Fence>(m_fence_resources.size());
			std::transform(m_fence_resources.begin(), m_fence_resources.end(),
				fences.begin(),
				[](auto pair) {return pair.first; });

			auto res = m_device.waitForFences(fences, true, std::numeric_limits<uint64_t>::max());
			assert(res == vk::Result::eSuccess);
		}
	}

	resource_manager(const resource_manager&) = delete;
	resource_manager(resource_manager&&) = default;
	resource_manager& operator=(const resource_manager&) = delete;
	resource_manager& operator=(resource_manager&&) = default;

	void add_task(vk::Fence fence, Resource resource) {
		m_fence_resources.emplace(fence, resource);
	}

	auto running_task_count() {
		return m_fence_resources.size();
	}

	bool exist_task_complemented() {
		std::vector<vk::Fence> fences(m_fence_resources.size());
		std::transform(m_fence_resources.begin(), m_fence_resources.end(), fences.begin(),
			[](auto pair) {
				return pair.first;
			});
		return vk::Result::eSuccess == m_device.waitForFences(fences, false, 0);
	}

	void recycle() {
		auto released_fences = std::vector<vk::Fence>{};
		for (auto [fence, resource] : m_fence_resources) {
			if (vk::Result::eSuccess == m_device.waitForFences({ fence }, true, 0)) {
				free(resource);
				released_fences.emplace_back(fence);
			}
		}
		for (auto fence : released_fences) {
			m_fence_resources.erase(fence);
			m_fence_manager.free_fence(fence);
		}
	}

	auto allocate_resource() {
		auto fence = m_fence_manager.allocate_fence();
		auto resource = m_resource_allocator.allocate();
		return std::pair{ fence, resource };
	}

	void free(Resource resource) {
		m_resource_allocator.free(resource);
	}
private:
	vk::Device m_device;
	fence_manager m_fence_manager;
	ResourceAllocator m_resource_allocator;
	std::unordered_map<VkFence, Resource> m_fence_resources;
};

using task_manager = resource_manager<vk::DescriptorSet, descriptor_set_manager>;

class queue_manager {
public:
	queue_manager(vk::Device device, vk::Queue queue, vk::CommandPool command_pool)
		:
		m_device{ device },
		m_queue{ queue },
		m_resource_manager{ device, command_buffer_manager{device, command_pool}, fence_manager{device} }
	{
		begin_record();
	}
	~queue_manager() {
		end_recording(nullptr);
	}

	void add_fence(vk::Fence fence) {
		m_recording_need_signal_fences.emplace_back(fence);
	}
	auto current_command_buffer() {
		return m_recording_command_buffer;
	}

	void begin_record() {
		m_resource_manager.recycle();
		auto [fence, command_buffer] = m_resource_manager.allocate_resource();
		m_resource_manager.add_task(fence, command_buffer);
		m_recording_need_signal_fences.emplace_back(fence);
		m_recording_command_buffer = command_buffer;
		m_recording_command_buffer.begin(vk::CommandBufferBeginInfo{});
	}
	void end_recording(vk::Semaphore semaphore) {
		m_recording_command_buffer.end();
		auto queue = m_queue;
		queue.submit(
			{
			vk::SubmitInfo{}.setCommandBuffers(m_recording_command_buffer)
				.setSignalSemaphores(semaphore).setSignalSemaphoreCount(semaphore != nullptr)
			});
		for (auto fence : m_recording_need_signal_fences) {
			queue.submit(
				{},
				fence
			);
		}
		m_recording_need_signal_fences.clear();
	}
	auto cycle_recording(vk::Semaphore semaphore) {
		end_recording(semaphore);
		begin_record();
	}

	void wait(vk::Semaphore semaphore, vk::PipelineStageFlags flags) {
		cycle_recording(nullptr);
		m_queue.submit(
			vk::SubmitInfo{}.setWaitSemaphores(semaphore).setWaitDstStageMask(flags)
		);
	}

private:
	vk::Device m_device;
	vk::Queue m_queue;
	resource_manager<vk::CommandBuffer, command_buffer_manager> m_resource_manager;

	vk::CommandBuffer m_recording_command_buffer;
	std::vector<vk::Fence> m_recording_need_signal_fences;
};
}