import std/[unittest, tables]
import ../src/nemo/core/types
import ../src/nemo/core/process

suite "Green Threads - Process and Scheduler":

  test "Create scheduler":
    let sched = newScheduler()
    check sched.processCount == 0
    check sched.readyCount == 0
    check not sched.isRunning

  test "Create process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("test-process")

    check proc1.pid == 1
    check proc1.name == "test-process"
    check proc1.state == psReady

  test "Add process to scheduler":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)

    check sched.processCount == 1
    check sched.readyCount == 1
    check sched.getProcess(proc1.pid) == proc1

  test "Select next process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")
    let proc2 = sched.newProcess("proc2")

    sched.addProcess(proc1)
    sched.addProcess(proc2)

    check sched.readyCount == 2

    let selected = sched.selectNextProcess()
    check selected == proc1
    check selected.state == psRunning
    check sched.currentProcess == proc1
    check sched.readyCount == 1

  test "Yield current process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)
    discard sched.selectNextProcess()

    check proc1.state == psRunning
    check sched.readyCount == 0

    sched.yieldCurrentProcess()

    check proc1.state == psReady
    check sched.readyCount == 1

  test "Block and unblock process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)
    discard sched.selectNextProcess()

    check proc1.state == psRunning

    let condition = WaitCondition(kind: wkMonitor, target: nil)
    sched.blockProcess(proc1, condition)

    check proc1.state == psBlocked
    check sched.blockedCount == 1
    check sched.readyCount == 0

    sched.unblockProcess(proc1)

    check proc1.state == psReady
    check sched.blockedCount == 0
    check sched.readyCount == 1

  test "Suspend and resume process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)

    sched.suspendProcess(proc1)
    check proc1.state == psSuspended

    sched.resumeProcess(proc1)
    check proc1.state == psReady
    check sched.readyCount == 2  # Was in ready queue, now added again

  test "Round-robin scheduling":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")
    let proc2 = sched.newProcess("proc2")
    let proc3 = sched.newProcess("proc3")

    sched.addProcess(proc1)
    sched.addProcess(proc2)
    sched.addProcess(proc3)

    # First round
    var selected = sched.selectNextProcess()
    check selected == proc1
    sched.yieldCurrentProcess()

    selected = sched.selectNextProcess()
    check selected == proc2
    sched.yieldCurrentProcess()

    selected = sched.selectNextProcess()
    check selected == proc3
    sched.yieldCurrentProcess()

    # Second round - back to proc1
    selected = sched.selectNextProcess()
    check selected == proc1

  test "Terminate process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)
    discard sched.selectNextProcess()

    sched.terminateProcess(proc1)
    check proc1.state == psTerminated

  test "Remove process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)
    check sched.processCount == 1

    sched.removeProcess(proc1.pid)
    check sched.processCount == 0
    check sched.getProcess(proc1.pid) == nil

  test "Run one slice with no processes":
    let sched = newScheduler()
    let ran = sched.runOneSlice()
    check not ran

  test "Run one slice with process":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")

    sched.addProcess(proc1)
    let ran = sched.runOneSlice()

    check ran
    check sched.currentProcess == proc1
    check proc1.state == psRunning

  test "List processes":
    let sched = newScheduler()
    let proc1 = sched.newProcess("proc1")
    let proc2 = sched.newProcess("proc2")

    sched.addProcess(proc1)
    sched.addProcess(proc2)

    let list = sched.listProcesses()
    check list.len == 2

  test "Scheduler with shared globals":
    var globals = new(Table[string, NodeValue])
    globals[] = initTable[string, NodeValue]()
    globals[]["testVar"] = NodeValue(kind: vkInt, intVal: 42)

    let sched = newScheduler(globals = globals)

    check not sched.sharedGlobals.isNil
    check "testVar" in sched.sharedGlobals[]
    check sched.sharedGlobals[]["testVar"].kind == vkInt
    check sched.sharedGlobals[]["testVar"].intVal == 42
