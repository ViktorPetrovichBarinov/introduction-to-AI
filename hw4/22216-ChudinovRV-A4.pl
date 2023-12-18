%Скорее всего подойдёт любой шрифт у которого все буквы одинакого
% размера, но я использовал Microsoft JhengHei размер 12

:- use_module(library(clpfd)).

:- use_module(large_data).
:- use_module(print).
:- use_module(utils).
:- use_module(cost).
%для измерения времени
measure_time(Goal, Time) :-
    statistics(runtime, [Start|_]),
    call(Goal),
    statistics(runtime, [End|_]),
    Time is End - Start.

%Точка входа
main() :-
    write('===============\n'),
    write('||Environment prepare ||\n'),
    write('===============\n'),
    clean,
    findall(Exam_ID, exam(Exam_ID, _), Exam_IDs),
    findall((Room_ID, Day, From, To), classroom_available(Room_ID, Day, From, To), Rooms_info),
    prepare_env(Exam_IDs),
    length(Exam_IDs, Number_of_exams),

    write('===============\n'),
    write('||   Start of processing   ||\n'),
    write('===============\n'),
    measure_time(main_loop(node([], 0, 0), Number_of_exams, Exam_IDs, Rooms_info, Best_schedule), Time),
    write('===============\n'),
    format('Time: ~w~n', [Time]),

    first_node(Best_schedule, Cheapest_event),
    first_cost(Best_schedule, Cheapest_cost),
    pretty_print(schedule(Cheapest_event)),
    format('All cost: ~w~n', [Cheapest_cost]),
    clean.


%Запускаем обработку имеющихся данных.
% В данном предикате мы выходим из рекцрсии, когда количество
% обработанных экзаменов сравняется с количеством экзаменов.
% На каждой итерации мы храним самый дешёвый вариант текущей длинны.
% Ну и штраф тоже.
main_loop(node(State, Number_of_exams, Cost), Number_of_exams, _, _, node(State, Number_of_exams, Cost)) :- !.

main_loop(node(State, Length, Cost), Number_of_exams, Exam_IDs, Room_IDs, Result) :-
    print_process(Length, Number_of_exams),
    add_event(Exam_IDs, Room_IDs, State, [], Events),
    create_new_states(node(State, Length, Cost), Events, node([], 0, 0), Min_node),
    main_loop(Min_node, Number_of_exams, Exam_IDs, Room_IDs, Result).


% Данный предикат выводит текущий прогресс обработки данных.
% Вывод происходит в виде A% | 100%
print_process(Length, Number_of_exams) :-
    Completed is Length / Number_of_exams,
    Completed_procents is Completed * 100,
    format('||   ~`0t~1f~10| %   |    ~w % || ~n', [Completed_procents, 100.0]).


% Данный предикт ответственнен за добавление новых состояний к уже
% существующим
add_event([], _, _, Acc, Acc).
add_event([Exam_ID|Rest], Rooms_info, State, Acc, Result) :-
    all_possible_exam_times(Exam_ID, Rooms_info, State, [], New_states),
    append(Acc, New_states, Updated_states),
    !,
    add_event(Rest, Rooms_info, State, Updated_states, Result).


%Данный предикат для каждого экзамена
%сопоставляет его положение и время
all_possible_exam_times(_, [], _, Acc, Acc).
all_possible_exam_times(Exam_ID, [(Room_ID, Day, From, To)|Rest], State, Acc, Result) :-
    exam_duration(Exam_ID, Duration),
    Start_time is To - Duration,
    generate_feasible_exam_times(event(Exam_ID, Room_ID, Day, From), Start_time, State, [], New_states),
    append(New_states, Acc, Updated_states),
    !,
    all_possible_exam_times(Exam_ID, Rest, State, Updated_states, Result).


% Данный предикат возвращает все ивенты с идентичными id-ками экзаменов,
% аудиторией и днями, но с разным временем.
generate_feasible_exam_times(event(_, _, _, From), Till, _, Acc, Acc) :-
    From #> Till.

generate_feasible_exam_times(event(Exam_ID, Room_ID, Day, From), Till, State, Acc, Result) :-
    not(is_event_possible(event(Exam_ID, Room_ID, Day, From), State)),
    !,
    New_from #= From + 1,
    generate_feasible_exam_times(event(Exam_ID, Room_ID, Day, New_from), Till, State, Acc, Result).

generate_feasible_exam_times(event(Exam_ID, Room_ID, Day, From), Till, State, Acc, Result):-
    is_event_possible(event(Exam_ID, Room_ID, Day, From), State),
    !,
    New_from #= From + 1,
    generate_feasible_exam_times(event(Exam_ID, Room_ID, Day, New_from),
                         Till,
                         State,
                         [event(Exam_ID, Room_ID, Day, From)|Acc], Result).


% Данный предикат проверяет, что некоторый ивент можно разместить в уже
% созданный и чем-то заполненный список событий.
is_event_possible(event(Exam_ID, Room_ID, Day, Hour), Events) :-
    ex_season_starts(Start_day), %первый день экзов
    ex_season_ends(End_day), %последний день экзов
    between(Start_day, End_day, Day), %проверка, что экзамен в промежутке

    classroom_available(Room_ID, Day, From, To), % получаем с какого по какое доступен класс
    exam_duration(Exam_ID, Duration), % получаем длительность экзамена
    classroom_capacity(Room_ID, Capacity), % вместимость класса по людям
    st_group(Exam_ID, Students),
    length(Students, Number_of_students),
    Number_of_students =< Capacity,

    Exam_lasts #= To - Duration,
    between(From, Exam_lasts, Hour), % можно ли провести экзамен  вданные часы
    not(intersection_check(event(Exam_ID, Room_ID, Day, Hour), Events)).

%is_accommodates(_, []) :- !, fail.
%is_accommodates(event(Exam_ID, Room_ID, Day, From),
%    [event(Exam_ID0,Room_ID0,Day0,From0)|T]) :-

% Предикат верен тогда и только тогда, когда есть любое пересечение
intersection_check(_, []) :- !, fail.
intersection_check(event(Exam_ID, Room_ID, Day, From),
    [event(Exam_ID0,Room_ID0,Day0,From0)|T] ) :-
            Exam_ID == Exam_ID0;
            (
                Day == Day0,
                exam_duration(Exam_ID, Duration),
                exam_duration(Exam_ID0, Duration0),
                To #= From + Duration,
                To0 #= From0 + Duration0,
                time_check(From, To, From0, To0),
                           ( student_follows_both_classes(Exam_ID, Exam_ID0);
                             teacher_teaches_both_classes(Exam_ID, Exam_ID0);
                             Room_ID == Room_ID0));
                intersection_check(event(Exam_ID, Room_ID, Day, From), T).



%Данный предикат в 4 аргумент ложит новое самое дешевое расписание.
% Предикт берёт наидешовейшее расписание длинны N с N-ой итерации и
% исправляет его, прибавляя state и по новой находит расписание с самым
% маленьким штрафом
%
create_new_states(_, [], Min_node, Min_node).
create_new_states(node(State, Length, Cost), [Event|Rest], node(Second_state, Second_length, Second_cost), Result) :-
    cost(schedule([Event|State]), New_cost),
    New_length #= Length + 1,
    min_cost(node([Event|State], New_length, New_cost),
               node(Second_state, Second_length, Second_cost), Min_node),
    create_new_states(node(State, Length, Cost), Rest, Min_node, Result).


% Предикат верен тогда и только тогда, когда есть пересечение по
% времени
time_check(From1, To1, From2, To2) :-
    (From2 =< From1, From1 =< To2);
    (From1 =< From2, From2 =< To1).

dinner_check(From, To) :-
    From =< 13;
    To >= 12.




%Данный предикат выдаёт расписание с минимальным штрафом
min_cost(node(_, Length, Cost), node(State2, Length, Cost),
    node(State2, Length, Cost)).
min_cost(node(_, Length1, _), node(State2, Length2, Cost2), node(State2, Length2, Cost2)) :-
    Length1 < Length2.
min_cost(node(State1, Length1, Cost1), node(_, Length2, _), node(State1, Length1, Cost1)) :-
    Length1 > Length2.
min_cost(node(State1, Length, Cost1), node(_, Length, Cost2), node(State1, Length, Cost1)) :-
    Cost2 > Cost1.
min_cost(node(_, Length, Cost1), node(State2, Length, Cost2), node(State2, Length, Cost2)) :-
    Cost2 < Cost1.


first_node(node(State, _, _), State).
first_cost(node(_, _, Cost), Cost).

