definition module TestFramework.TaskTests

// Glue code between TestFramework and iTasks
// Import this module if you want to test tasks.
// Import only TestFramework if you want to test pure and IO functions.

import TestFramework
import iTasks

:: TaskTestcase = TaskTestcase String (Task TestResult)

derive class iTask TestResult

runTaskTests :: [TaskTestcase] *World -> *World
