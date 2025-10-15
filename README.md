These are the source files of the first part of the OCaml assignment for the Programming Paradigms course (CPS2001) 

---

# Assignment one instructions

All the source files that you need to modify and implement are located in the `assignmentone/lib/worksheet.ml` directory.

Replace the lines `failwith "TODO Implementation"` in the `worksheet.ml` module with your implementation.

Tests have been provided to validate your implementation. The tests can be found in the `assignmentone/test/test_worksheet.ml` directory. **Do not modify** files in this module.

Run `dune exec bin/main.exe` to confirm that everything works. You should obtain the following on your terminal:

```shell
> dune exec bin/main.exe
Assignment one worksheet!  
```

# Handy command line shortcuts

- To **compile** your source, `cd` to the `assignmentone` directory and execute:
  ```shell
  > dune build
  ```
  Built files are placed in the `_build` directory. These files are overwritten on each build.

- To **clean** your previous build, which might be necessary in case of funny errors, execute
  ```shell
  > dune clean
  ```

- To run your **tests**, execute:
  ```shell
  > dune test
  ```
  The result of tests should be shown in the terminal, including errors and the name of the test that failed.

  When all tests are successful, `dune test` should output:
  ```shell
  > dune test
  .....................              
  Ran: 21 tests in: 0.11 seconds.
  OK
  ```
  on your terminal.
  