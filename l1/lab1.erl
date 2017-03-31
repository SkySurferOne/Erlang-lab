-module(lab1).
-export[power/2, contains/2, duplicateElements/1, divisibleBy/2,
        toBinary/1].

power(_, 0) -> 1;
power(A, N) -> A * power(A, N - 1) .

contains([], _) -> flase;
contains([A|T], A) -> true;
contains([_|T], A) -> contains(T, A).

duplicateElements([]) -> [];
duplicateElements([H|T]) -> [H, H] ++ duplicateElements(T).

divisibleBy([], _) -> [];
divisibleBy([H|T], D)
  when (H rem D == 0) -> [H] ++ divisibleBy(T, D);
divisibleBy([H|T], D)
  when (H rem D /= 0) -> divisibleBy(T, D) .

toBinary(0) -> [];
toBinary(N) -> toBinary(N div 2) ++ [N rem 2].
