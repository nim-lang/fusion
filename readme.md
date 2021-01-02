# Nim Fusion

**Docs:** https://nim-lang.github.io/fusion/theindex.html

Fusion contains Nim modules that are to be bundled with the Nim installation in
order to give us something like the "Nim distribution".
Fusion fullfills multiple purposes:

* It contains candidates for inclusion into the stdlib. However these modules only
  move into the stdlib if it makes sense to maintain them with the rest of the
  core of Nim and currently I cannot imagine why that ever would need to be the case.
* Fusion has a broader perspective about what is considered useful.
  For example an HTML parser or a simple UI library that doesn't support every
  OS that Nim itself supports is acceptable for Fusion.
* Fusion aims to be an *immutable* code repository, once a module is included it stays.
  Like Nim itself, Fusion uses a `.since` annotation. New modules which contain
  few procs are preferred over larger modules that have more procs.
* Fusion is also a Nimble package and it is compatible with Nim version 1 as well
  as the latest Nim.
* Fusion uses semver but does not go beyond major version 1 since it's also about
  preserving the past. Instead of breaking changes for a given module M.nim,
  a module named M2.nim will be introduced.
  M2.nim can share code with M.nim via Nim's `include` or `import export`
  mechanism in order to fight code duplication.

Fusion is for now an idea about how to grow Nim's ecosystem without the pain points
of more traditional approaches.
Time will tell if the idea is a good one.
