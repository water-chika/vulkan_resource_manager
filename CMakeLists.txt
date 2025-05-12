cmake_minimum_required(VERSION 3.20)

project(vulkan_resource_manager)

add_library(vulkan_resource_manager
    vulkan_resource_manager.hpp
    vulkan_resource_manager.cpp)
target_compile_features(vulkan_resource_manager PUBLIC cxx_std_20)
target_include_directories(vulkan_resource_manager PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(vulkan_resource_manager PUBLIC Vulkan::Vulkan)

add_executable(vulkan_resource_manager_test
    main.cpp)
target_link_libraries(vulkan_resource_manager_test PUBLIC vulkan_resource_manager)
