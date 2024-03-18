// This source code is licensed under the
// Creative Commons Zero v1.0 Universal License
// (CC0 1.0 Universal / CC0 1.0)
// https://creativecommons.org/publicdomain/zero/1.0/legalcode

// Author: Lion Kortlepel / @lionkor / development@kortlepel.com

#include "threadpool.h"
#include <iostream>

int main() {
    ThreadPool pool { 8 };

    pool.add_task([] {
        std::cout << "Hello from " << std::this_thread::get_id() << std::endl;
    });

}
