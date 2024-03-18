// This source code is licensed under the
// Creative Commons Zero v1.0 Universal License
// (CC0 1.0 Universal / CC0 1.0)
// https://creativecommons.org/publicdomain/zero/1.0/legalcode

// Author: Lion Kortlepel / @lionkor / development@kortlepel.com

#pragma once

#include <condition_variable>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>
#include <iostream>

using Task = std::function<void()>;

class ThreadPool {
public:
    /// constructs the threadpool with n_threads threads
    explicit ThreadPool(size_t n_threads);

    /// blocks the destruction until all tasks are finished
    ~ThreadPool();

    /// adds a task to be executed
    void add_task(Task task);

private:
    void thread_main();

    std::queue<Task> m_tasks; // очередь задач
    std::mutex m_tasks_mtx; // мьютекс
    std::condition_variable m_tasks_cnd; // усл переменная
    std::vector<std::thread> m_threads; // потоки
    bool m_shutdown { false };
    size_t done_tasks_count = 0;
};
