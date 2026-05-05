; -*- domain-file: t; -*-

(define (domain ste-test-domain)
 (:requirements :adl)
 (:types
   mem-block - addressable
   process - object
   address - object
   pointer - mem-block
   sysctl-table-entry - addressable
   addressable - object
   mem-len - object
   procname - object)
 (:constants
   ps-not-started - object
   ps-running - object
   ps-clean-exit - object
   ps-crash - object
   mem-block-core-pattern-buffer - mem-block
   mem-block-core-pattern-buffer-plus-2 - mem-block
   corrupt-path - object
   string_kernel_dot_core_pattern - procname)
 (:predicates (mem-block ?mb - mem-block) (block-size ?mb - mem-block)
  (attacker-controls ?mb - mem-block)
  (mem-block-contents ?mb - mem-block ?s - object)
  (proc-contains ?proc - process ?mb - mem-block)
  (proc-state ?proc - process ?ps - object) (address-p ?a - address)
  (ste-procname ?ste - sysctl-table-entry ?pn - procname)
  (ste-data-addr ?ste - sysctl-table-entry ?b - mem-block)
  (ste-data-mem-block ?ste - sysctl-table-entry ?b - mem-block)
  (ste-maxlen ?ste - sysctl-table-entry ?len - mem-len)
  (addressable ?obj - addressable) (addr ?obj - addressable ?a - address)
  (know-addr-of ?obj - addressable) (dmesg-observable ?obj - addressable)
  (ace2-used) (have-root) (dmesg-running) (waiting-to-observe ?thing - object)
  (win) (attacker-owns-core-dump ?process - process))
 (:action compute-sysctl-table-entry-addr :parameters
  (?ste - sysctl-table-entry) :precondition
  (and
    (have-root)
    (dmesg-running)
    (not (know-addr-of ?ste)))
  :effect
  (and
    (forall (?x - pointer)
      (when
        (ste-data-addr ?ste ?x)
        (know-addr-of ?x)))
    (know-addr-of ?ste)))
 (:action compute-memory-block-addr-using-dmesg :parameters
  (?memory-block - mem-block) :precondition
  (and
    (have-root)
    (dmesg-running)
    (dmesg-observable ?memory-block))
  :effect
  (and
    (know-addr-of ?memory-block)))
 (:action compute-mem-block-addr-core-pat-plus-2
   :parameters
   (?core-pat-buff - mem-block ?core-pat-buff-plus-2 - mem-block)
   :precondition
   (and
    (= ?core-pat-buff mem-block-core-pattern-buffer)
    (= ?core-pat-buff-plus-2 mem-block-core-pattern-buffer-plus-2)
    (know-addr-of ?core-pat-buff))
   :effect
   (and
    (know-addr-of ?core-pat-buff-plus-2)))
 (:action ace2-set-pointer-to-buffer :parameters
  (?ptr - pointer ?mb - mem-block ?addr-mb - address) :precondition
  (and
    (know-addr-of ?ptr)
    (know-addr-of ?mb)
    (not (ace2-used))
    (addr ?mb ?addr-mb))
  :effect
  (and
    (mem-block-contents ?ptr ?addr-mb)
    (ace2-used)))
 (:action sysctl-write
   :parameters
  (?procname - procname
             ?new-contents - object
             ?ste - sysctl-table-entry
             ?ptr - pointer
             ?mb - mem-block
             ?a - address)
  :precondition
  (and
    (ste-procname ?ste ?procname)
    (ste-data-addr ?ste ?ptr)
    (mem-block-contents ?ptr ?a)
    (addr ?mb ?a)
    (know-addr-of ?mb)
    (not (= ?procname string_kernel_dot_core_pattern)))
  :effect
  (and
    (mem-block-contents ?mb ?new-contents)))
 (:action kill-process :parameters (?process - process) :precondition
  (and
    (mem-block-contents mem-block-core-pattern-buffer-plus-2 corrupt-path)
    (proc-state ?process ps-running))
  :effect
  (and
    (proc-state ?process ps-crash)
    (attacker-owns-core-dump ?process))))