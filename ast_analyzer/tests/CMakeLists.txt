cmake_minimum_required(VERSION 3.10)

# set the project name
project(test VERSION 0.1)


include(FetchContent)

add_executable(
    main 
    test_serialize.cpp
)

FetchContent_Declare(json URL https://github.com/nlohmann/json/releases/download/v3.11.2/json.tar.xz)
FetchContent_MakeAvailable(json)

target_link_libraries(main PRIVATE nlohmann_json::nlohmann_json)

