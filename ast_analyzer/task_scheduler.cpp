// #include <vector>
// #include "base_analyzer.hpp"
// #include <functional>
// #include <algorithm>

// typedef struct task {
//   pthread_barrier_t barrier ;
//   std::vector<base_analyzer *> analyzers_;
// } task;

// void* execute_task(void* args) {
//   task* t = (task*) (args);

//   for (auto analyzer : t->analyzers_) {
//       std::cerr << analyzer->get_name() << std::endl;
//       analyzer->analyze();
//       delete analyzer;
//   }

//   pthread_barrier_wait(&t->barrier);
//   pthread_exit(NULL);
// }

// class task_scheduler {
// private:
//   std::vector<base_analyzer *> analyzers_;
//   const int PTHREAD_CREATE_ERROR = 0;
//   const int PTHREAD_JOIN_SUCCESS = 0;
//   const int THREADS_ONLY = 0;
//   pthread_barrier_t barrier ;

// public:

//   task_scheduler(const std::vector<base_analyzer *>& analyzers, pthread_barrier_t barrier) 
//     : analyzers_(analyzers), barrier(barrier) {
//   }

//   void execute_tasks() {
//     pthread_t threads[2];
//     std::vector<base_analyzer*> first_task;
//     std::vector<base_analyzer*> second_task;

//     first_task.assign(analyzers_.begin(), analyzers_.begin() + 3);
//     second_task.assign(analyzers_.begin() + 3, analyzers_.end());

//     auto first = task{barrier, first_task};
//     auto second = task{barrier, second_task};

//     int create_result = pthread_create(&threads[0], NULL, execute_task, (void *)(&first));

//     if (create_result != PTHREAD_CREATE_ERROR) {
//         printf("pthread_create error: couldn't create thread\n");
//         return;
//     }

//     create_result = pthread_create(&threads[1], NULL, execute_task, (void *)(&second));
//     if (create_result != PTHREAD_CREATE_ERROR) {
//         printf("pthread_create error: couldn't create thread\n");
//         return;
//     }

//   }
// };


