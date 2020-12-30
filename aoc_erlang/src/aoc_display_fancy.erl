-module(aoc_display_fancy).

-behavior(aoc_display).

-include_lib("stdlib/include/assert.hrl").

% Behavior exports
-export([ init/1
        , update_step_time/4
        , update_part_result/4
        , summarize/2
        ]).

% Internal exports
-export([ default_fmt_fun/1
        ]).

-include("aoc_puzzle.hrl").

-type align() :: left | center | right.

-define(DEFAULT_WIDTH, 10).

%% Maps fields in #puzzle{} to the actual value to write in the cell
-type value_fun() :: fun((Puzzle :: aoc_puzzle()) -> term()).

%% Formats values (align, precision, etc)
-type fmt_fun() :: fun((Value :: term()) -> string()).

-type row_data() :: aoc_puzzle() | header.

-record(column,
        { label :: string()
        , width = ?DEFAULT_WIDTH :: integer()
        , field :: atom()
        , value_fun :: value_fun()
        , fmt_fun = fun default_fmt_fun/1 :: fmt_fun()
        , align = left :: align()
        , header_align = left :: align()
        }).

-type column() :: #column{}.

-record(display, { columns = [] :: [column()]
                 , puzzles :: aoc_puzzle_map()
                 , row_order :: [aoc_puzzle_id()]
                 }).

-type display() :: #display{}.

-define(FIELD_FUN(F), fun(P) -> P#aoc_puzzle.F end).

-spec init(Puzzles :: aoc_puzzle_map()) -> {ok, display()}.
init(Puzzles) ->
  save(),
  try
    cls()
  after
    restore()
  end,
  Columns = [#column{label = "Year",    width = 4,        value_fun = ?FIELD_FUN(year)},
             #column{label = "Day",     width = 4,        value_fun = ?FIELD_FUN(day)},
             #column{label = "Name",    width = 20,       value_fun = ?FIELD_FUN(name)},
             #column{label = "Parsing",                   value_fun = ?FIELD_FUN(parse)},
             #column{label = "Part 1",                    value_fun = ?FIELD_FUN(part1)},
             #column{label = "Part 2",                    value_fun = ?FIELD_FUN(part2)},
             #column{label = "Part 1 result", width = 15, value_fun = ?FIELD_FUN(part1_result)},
             #column{label = "Part 2 result", width = 15, value_fun = ?FIELD_FUN(part2_result)},
             #column{label = "Part 1 status", width = 15, value_fun = fun(P) -> element(1, P#aoc_puzzle.status) end},
             #column{label = "Part 2 status", width = 15, value_fun = fun(P) -> element(2, P#aoc_puzzle.status) end}
            ],

  DisplayState = #display{columns = Columns,
                          puzzles = Puzzles,
                          row_order = lists:sort(maps:keys(Puzzles))},

  draw_table(DisplayState),
  {ok, DisplayState}.

-spec update_step_time(PuzzleId :: aoc_puzzle_id(),
                       Part :: parse | part1 | part2,
                       Time :: integer(),
                       Display :: display()) -> {ok, display()}.
update_step_time(_PuzzleId, _Part, _Time, Display) ->
  %% io:format("~p ~p ~p~n", [PuzzleId, Part, Time]),
  {ok, Display}.

-spec update_part_result(PuzzleId :: aoc_puzzle_id(),
                         Part :: parse | part1 | part2,
                         Result :: term(),
                         Display :: display()) -> {ok, display()}.
update_part_result(_PuzzleId, _Part, _Result, Display) ->
  %% io:format("~p ~p ~p~n", [PuzzleId, Part, Result]),
  {ok, Display}.

-spec summarize(Puzzles :: #{aoc_puzzle_id() => aoc_puzzle()},
               Display :: display()) -> ok.
summarize(_Puzzles, _Display) ->
  %% io:format("Summarizing: ~p~n", [Puzzles]),
  ok.


%%%=============================================================================
%%% Helpers
%%%=============================================================================

-define(CSI, "\033[").
-define(CLEAR_SCREEN, ?CSI "2J").

-define(BLACK, $x).
-define(GREEN, $g).
-define(RED, $r).
-define(YELLOW, $y).
-define(BLUE, $b).
-define(MAGENTA, $m).
-define(WHITE, $w).
-define(CYAN, $c).

-define(BOLD(X), (X ($A - $a)))

-define(COLOR(Color, Str), "~!" ++ [Color] ++ Str ++ "~!!").
-define(BG(Color, Str), "~#" ++ [Color] ++ Str ++ "~!!").

-define(MOVE_UP, ?CSI "1F").

draw_table(DisplayState) ->
  save(),
  try
    %% io:format("~p~n", [DisplayState])
    draw_table0(DisplayState)
  after
    restore()
  end.

draw_table0(#display{row_order = Rows,
                     columns = Columns} = DisplayState) ->
  AllRows = [header|Rows],
  NumRows = length(AllRows),
  move_up(NumRows),

  lists:foreach(fun(RowData) ->
                    draw_row(RowData, Columns, DisplayState),
                    move_down(1)
                end, AllRows).

-spec draw_row(RowData :: row_data(), [column()], display()) -> ok.
draw_row(RowData, Columns, State) ->
  lists:foldl(
    fun(Column, X) ->
        draw_cell(RowData, Column, X, State)
    end, 1, Columns),
  ok.


-spec draw_cell(row_data(), column(), integer(), display()) -> integer().
draw_cell(RowData,
          #column{label = Label,
                  value_fun = ValueFun,
                  fmt_fun = FmtFun,
                  header_align = HAlign,
                  align = RowAlign,
                  width = Width
                 } = _Column, X,
          #display{puzzles = Puzzles} = _State) ->

  Value = case RowData of
            header -> Label;
            {_Year, _Day} = Id ->
              Puzzle = maps:get(Id, Puzzles),
              ValueFun(Puzzle)
          end,

  FormattedText =
    case RowData of
      header -> Value;
      _ -> FmtFun(Value)
    end,

  Align =
    case RowData of
      header -> HAlign;
      _ -> RowAlign
    end,


  move_to_abs_x(X),
  Str = format_cell(Width, Align, FormattedText),
  ?assertEqual(length(Str), Width),
  io:format("~s", [Str]),
  X + Width + 1.

format_cell(Width, Align, Str) ->
  lists:flatten(
    case Align of
      left ->  io_lib:format("~-*..*s", [Width, $ , Str]);
      right -> io_lib:format("~*..*s", [Width, $ , Str]);
      center ->
        N = length(Str),
        if N > Width ->
            lists:sublist(Str, Width);
           N == Width ->
            Str;
           N < Width ->
            Space = Width - N,
            L = Space div 2,
            R = Space - L,
            lists:duplicate(L, " ") ++ Str ++
              lists:duplicate(R, " ")
        end
    end).

move_up(N) ->
  io:format(?CSI "~pF", [N]).

move_to_abs_x(X) ->
  io:format(?CSI "~pG", [X]).

move_down(N) ->
  io:format(?CSI "~pE", [N]).

cls() ->
  io:format(?CLEAR_SCREEN, []).

save() ->
  io:format(?CSI  "s", []).

restore() ->
  io:format(?CSI  "u", []).

default_fmt_fun(Value) when is_list(Value) ->
  io_lib:format("~s", [Value]);
default_fmt_fun(undefined) ->
  "-";
default_fmt_fun(Value) ->
  io_lib:format("~p", [Value]).

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

format_cell_test_() ->
  [ ?_assertEqual("foo       ", format_cell(10, left, "foo"))
  , ?_assertEqual("   foo    ", format_cell(10, center, "foo"))
  , ?_assertEqual("       foo", format_cell(10, right, "foo"))
  , ?_assertEqual("foof",       format_cell(4, left, "foofoofoo"))
  ].

-endif.
