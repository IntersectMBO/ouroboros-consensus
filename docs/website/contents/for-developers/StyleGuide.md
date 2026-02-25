# Consensus style guide

This document describes the style we follow in the consensus team. While style
is subjective, we attempt to motivate our choices with *objective reasons* where
possible. Not everyone agrees with every choice we make, but we compromise. It
is unlikely that this style guide matches the preference of even a single team
member for 100%. In different teams, with different values and priorities,
different choices might make more sense.

Aesthetics is something that a lot of programmers feel strongly about, and the
goal of this style guide is not to stifle all personal freedom of expression. It
is an attempt to document the style that we have tried to adhere to as much as
we can in the codebase, and of course it is a style that we believe works well.
When modifying the existing code, it would therefore be much appreciated if
the existing style is adhered to, either by looking at the surrounding code or
by reading this style guide. Whenever possible, it would also be good to stick
to this style guide for new code. Should you disagree strongly with some of the
recommendations here, however, feel free to "carve out a little niche" for
yourself by adhering to your own preferences in modules that exclusively you
work on (if this means splitting some modules up, that is fine). However, please
do try to be consistent; inconsistent style is not a preference, that's just
sloppiness.

## Enforcement

As part of each PR review, we also check for consistency with the content of
this document. We find that the rules herein become familiar and intuitive after
some use: eventually it'll just be a document you refer to only occasionally.
But we don't expect the first several PRs to perfectly adhere to these rules. So
please make an effort, but don't worry too much: our highest priority is to see
your PR's content! We'll help tidy up any deviations.

For maintenance work in particular, it suffices to focus only on the PR's diff.
Our goal in that case is just to avoid obvious deviations from the module's
existing style choices. Specifically, it's OK to inspect only the diff itself
along with its context -- eg whatever is available in the GitHub PR interface.
As long that rendering doesn't show that the PR spoils something like
intentional alignment for example, then the PR has no style problems.

We run `fourmolu` as a requirement for merging. The specific
configuration can be found [here][fourmolu-config].

## Guiding principles

We value the following principles in the consensus team:

* __Optimise for clarity__: the following things occur with decreasing
  frequency:

  1. One *reads* the code and tries to understand it.
  2. After having understood the code, one *modifies* it.
  3. One *writes* the initial version of the code. This only happens once.

  We believe it is important to optimise for (1), then (2), but *not* for (3).
  Making a little effort to make to code easier to understand when writing or
  modifying it pays off dividends when reading it the next time, especially when
  another team member will be the one reading it. Picking good names, formatting
  it in a clear way, separating concerns, highlighting the differences between
  similar steps, documenting why it does this and that, ..., are all worth the
  extra effort.

* __Consistency__: inconsistent style, especially within a single module,
  looks sloppy, inspires little confidence in the quality of the code,
  and distracts. Consistency is also a helpful guiding factor when deciding
  on style guidelines in the first place; sometimes a choice between formatting
  something this way or that way seems arbitrary in isolation, but becomes
  clearer when seen as part of a coherent whole.

## Formatting

We now list the formatting rules we have converged on.

1. __Indentation__: we indent by 2 spaces.

   *Why:* to avoid wasting horizontal screen space.

2. __Line length__: we limit the number of characters per line to 100.

   *Why:* long lines are less readable (there is a reason why books and
   newspapers limit their line length). It's also practical: even with
   (ultra-)wide monitors, most people tend to have many windows side by side.

   If you are going beyond 100 characters, wrap the line, introduce local
   bindings, etc.

   Comments and docstrings should also be wrapped at 100 characters.

   There are a few exceptions:

   * Sometimes alignment trumps line length. When many lines are aligned and a
     few of them are too long because of that, the clarity that comes from
     alignment (emphasising differences and similarities) can outweigh the line
     length limit.

   * Diagrams, examples, or long URLs in the comments can be wider than 100
     characters.

3. __Parentheses__: avoid redundant parentheses, except when they help with the
   order of operations. Use your judgement, and aim for clarity. Redundant
   parentheses sometimes help the reader, but sometimes confuse as they
   might suggest that they are there to disambiguate something whereas in fact
   there is nothing to disambiguate.

   ```haskell
   -- NO
   foo (Bar x) = (Bar (succ x))
   -- YES
   foo (Bar x) = Bar (succ x)

   -- NO
   ((x + y), z)
   -- YES
   (x + y, z)

   -- OKAY
   (fromIntegral x) * y
   ```

4. __Spaces__: surround binary operators with a space on each side. A comma is
   *always* followed by a space.

   *Why:* this is a general convention that is also used in text and math books.
   Not doing so makes it harder to read and is sloppy.

   ```haskell
   avg x y = (x + y) / 2

   let ((x, y), z) = foo
   in (y, z)
   ```

   The only exception is in tuple sections:

   ```haskell
   (,) <$> foo <*> bar
   (True,) <$> foo
   ```

5. __Function composition and the dollar operator__:

   Choose between using parenthesis, `$` and `.` in whichever way you think
   results in the most readable code.

6. __Blank lines__: we use *exactly one blank line* between different
   declarations: export lists, import lists, declarations, etc.

   *Why:* a blank line helps with readability. Always using a single one is
   consistent and easier to adhere to than one line in these cases and two lines
   in those other cases.

   When defining multiple non-trivial bindings in a `where`-block, separate them
   with a single blank line.

   ```haskell
   fooBar .. =
       ..
     where
       foo :: ..
       foo = ..

       bar :: ..
       bar = ..

   -- OKAY
   foo .. =
     where
       x = ..
       y = succ x
   ```

   Always end a file with a *newline*, which is not the same as a blank line.
   ```
   -- NO
   ..

   <EOF>

   -- NO
   ..<EOF>

   -- YES
   ..
   <EOF>
   ```
   *Why:* see [this StackOverflow answer][posix-line], moreover, GitHub will
   highlight a missing newline at the end of the file.

   [posix-line]: https://stackoverflow.com/questions/729692/why-should-text-files-end-with-a-newline#answer-729795

7. __Sections__: we group related definitions in sections that start with a
   section title. The same grouping can be replicated in the export list.

    ```haskell
    module AmazingModule (
         -- Foo
        Foo (..)
      , mkFoo
        -- Bar
      , ..
      ) where

    {-------------------------------------------------------------------------------
      Foo
    -------------------------------------------------------------------------------}

    data Foo = ..

    mkFoo :: ..

    ..

    {-------------------------------------------------------------------------------
      Bar

      Bar is bla bla
    -------------------------------------------------------------------------------}

    type Bar = ..
    ..
    ```

    The two lines of the section header are each 80 characters in total. The
    title is indented by two spaces. The section header can contain more text,
    which is separated from the first line by one blank line. The section header
    has a single blank line above and below it.

8. __Comment style__: in general we tend to use `--` instead of `{- .. -}`. We
   sometimes make exceptions for big non-Haddock comments.

9. __Haddock formatting__: we use [Haddock formatting][haddock-formatting] in
    docstrings. We also do this in comments for consistency.

    ```haskell
    -- | Short title
    --
    -- Longer description .. 'Foo' .. "Data.String" .. @a@ .. /not/ ..
    -- __never__ .. called \"foo bars\" .. alternative style " foo bars "
    -- .. @'Foo' a@
    --
    -- > foo bar baz
    --
    -- .. ends here.
    foo :: ..
    ```

    Note the space after the `|`. We do not align the following lines with the
    first character of the `|`.

    Haddock treats something between double quotes as a link to a module. So
    when you try to quote something, either use backslashes or extra spaces as
    in the example above.

    We prefer `-- |` over `-- ^`. We only use the latter when documenting the
    arguments to a constructor or a function:

    ```haskell
    foo ::
         Word  -- ^ Max size
      -> ..

    data Foo =
        -- | Foo
        --
        -- ..
        Foo
         Int  -- ^ @x@
         (Maybe Bool)
         -- ^ .. long line ..

        -- | Baar
      | Baar
    ```

    Note the indentation of `-- |`, the two spaces before the `-- ^`, and the
    blank line between the constructors.

    We often document preconditions, invariants, and postcondition using the
    following style:

    ```haskell
    -- | Foo
    --
    -- PRECONDITION: x must be greater than y
    -- > x > y
    --
    -- POSTCONDITION: the result will be positive
    foo :: ..

    data Foo = Foo {
          -- | The bar ..
          fooBar :: Int

          -- | The baz ..
          --
          -- INVARIANT: 'fooBaz' is always greater than 7
        , fooBaz :: Int
        }
    ```

    [haddock-formatting]: https://www.haskell.org/haddock/doc/html/ch03s08.html

13. __case vs function with multiple clauses__:

    The choice between using a `case` and having multiple clauses of the
    function can help emphasise the structure of the code, and the differences
    and commonalities between the cases.

    ```haskell
    foo acc visited = \case
        []   -> ..
        x:xs -> ..
    ```

15. __Import lists__: we use `fourmolu` to automatically format import
    lists. See the [`fourmolu.yaml` config][fourmolu-config].

    When importing modules from consensus and in particular modules from the
    same package, an import list and a qualifier can be omitted. For example,
    importing `Ouroboros.Consensus.Block` is often done without an import list
    as it brings many basic definitions that are relied upon in scope.

    When importing from other packages, we prefer to use either an import list
    or a qualifier.

16. __Export lists__: we use `fourmolu` to automatically format export
    lists. See the [`fourmolu.yaml` config][fourmolu-config]. We format
    export lists in the following way:

    ```haskell
    module X
      (
        ..
      , ..
      ) where
    ```

    We sometimes use Haddock headings:

    ```haskell
    module X
      ( -- * Foo
        ..
        -- ** Foo Bar
      , ..
        -- * Bar
      , ..
      ) where
    ```

    When exporting something with members, e.g., a datatype with
    constructors or a class with methods, we format them in the following way
    (note the space):

    ```haskell
    module X
      ( Foo (..)
      , Bar (MkBar)
      ) where
    ```

    *Why:* this is consistent with how `fourmolu` formats it when
    importing it.

    When intentionally hiding the constructor of a datatype or newtype, we add
    a `-- opaque` comment after it in the export list to be explicit about this:

    ```haskell
    module X
      ( Foo -- opaque
      ) where
    ```

    *Why:* otherwise, people unfamiliar with this type might be tempted to
    export its constructor without realising they're hidden for a reason. This
    comment should make them (and the reviewer) think twice.

17. __Syntactic extensions__: we like to use some syntactic language extensions.
    Some argue against having to learn additional syntax, but we believe the
    learning curve is minimal and using them can help improve the clarity of the
    code.

    We like to use `LambdaCase` to avoid giving intermediate results a redundant
    name:

    ```haskell
    -- OKAY
    mFoo <- getFoo
    case mFoo of
      Nothing  -> ..
      Just foo -> ..

    -- OKAY
    getFoo >>= \case
      Nothing  -> ..
      Just foo -> ..
    ```

    In the second snippet, there was no need to name the intermediary `mFoo`
    result. Especially when its name is long or coming up with a reasonable name
    for it is tricky, we recommend using `LambdaCase`.

    The use of `MultiWayIf` is also recommended when it improves the
    readability:

    ```haskell
    if | Set.member pt prevApplied -> Just True
       | Map.member hash invalid   -> Just False
       | otherwise                 -> Nothing
    ```

    In our opinion, this is more readable than alternatives like:

    ```haskell
    if Set.member pt prevApplied then Just True else
    if Map.member hash invalid   then Just False else
                                      Nothing
    ```

18. __Records__:

    We purposefully discourage the use of `RecordWildCards`.

    For records we often use `NamedFieldPuns` to make it convenient to extract
    fields from the record. We use the following convention when naming fields
    to avoid duplicate record fields (we do not use `DuplicateRecordFields`):

    ```haskell
    data SomeRecord = SomeRecord
        { someRecordA :: ..
        , someRecordB :: ..
        }
    ```

    To avoid long lines, it is sometimes useful to use record deconstruction in
    local bindings:

    ```haskell
    foo someRecord =
        ..
      where
        SomeRecord {someRecordA, someRecordB} = someRecord
    ```

    The convention above can be also contracted into `srA`, `srB`, etc, i.e.
    abbreviating the name of the data definition.

    We try to avoid partial fields, but replacing partial fields such as

    ```haskell
    data Foo = FooX {foo :: A, bar :: B} | FooY
    ```

    with

    ```haskell
    data Foo = FooX A B | FooY
    ```

    is _not_ an improvement: replacing record field names with positional
    arguments is a big loss in clarity. Instead, introduce a record to be used
    as an argument to the `FooX` constructor.

    ```haskell
    data X = X {foo :: A, bar :: B}
    data Foo = FooX X | FooY
    ```

19. __Pointfree__: Use your judgement when to use pointfree style and when not
    to use it; aim for clarity.

20. __Warnings__: we use the following warnings for each Cabal component:

    ```haskell
    -Wall
    -Wcompat
    -Wincomplete-uni-patterns
    -Wincomplete-record-updates
    -Wpartial-fields
    -Widentities
    -Wredundant-constraints
    -Wmissing-export-lists
    -Wunused-packages
    -Wno-unticked-promoted-constructors
    ```

    *Why:* the warnings produced by the above list of flags signal code smells
    or enforce good practices. There is seldom a reason to disable one of them.
    At the time of speaking, we haven't needed any CPP yet to accomplish this.

    We also keep the code entirely warning free; doing this consistently and
    without exception means that important warnings don't get lost. We enforce
    this by using `-Werror` in CI.

    We sometimes make exceptions for test code, e.g.,
    `-Wno-incomplete-uni-patterns`.

    For consistency, always use `-Wx` and `-Wno-x` instead of `-fwarn-x` and
    `-fno-warn-x`.

21. __HasCallStack__: when using `error` in code paths should be impossible and
    are indicative of bugs, make sure enough `HasCallStack` constraints are in
    scope so that the error message will result in a useful callstack.

    Note that `HasCallStack` constraints on record fields will need manual
    wrappers to work properly:

    ```haskell
    data API m = API {
          foo_ :: HasCallStack => Maybe a -> m a
        }
    foo :: HasCallStack => API m -> Maybe a -> m a
    foo = foo_
    ```

    Without the extra wrapper `foo`, the call stack would only start at `_foo`,
    which is rather useless.

22. __Ambiguous types__: we avoid `AllowAmbiguousTypes`. Instead, we add a
    `Proxy` argument for the ambiguous type variable.

    *Why:* this makes it explicit which type variable is ambiguous.

    When passing the `Proxy`, use `Proxy @X` where `X` is the concrete type.

    *Why:* this is less verbose than `Proxy :: Proxy X`.

    Generally try to avoid type applications, as they are rather brittle: if the
    type arguments to the function change order, suddenly a function call might
    no longer work, often with a hard to understand error message. This gets
    even worse when a function doesn't have an explicit `forall`, and so the
    order is not even specified. Prefer to use `Proxy`, possibly by introducing
    some auxiliary functions.

    When the same `Proxy` can be used multiple times, one can define it locally
    like so:

    ```haskell
    pb :: Proxy blk
    pb = Proxy
    ```

23. __Redundant pragmas__: remove unused language pragmas when possible.

    *Why:* if a module lists the `CPP`, `AllowAmbiguousTypes`,
    `UndecidableInstances`, or any other suspicious extension, it triggers an
    unnecessary red flag. Even for harmless extensions, it is good practice to
    remove unused ones.

    *Tip:* HLint can warn you about some unused pragmas.

24. __Reexports__:
     When re-exporting several modules from one module, use the following pattern:

    ```haskell
    module Foo (
        fooA
      , fooB
      , fooC
      , ...
      ) where

    import Foo.A (fooA, ...)
    import Foo.B (fooB, ...)
    import Foo.C (fooC, ...)

    ```

    *Why:* this leads to more changes to the export list, but makes it
    absolutely clear where each identifier comes from.

## Guidelines

There are more general guidelines on how we write and structure code.

1. __Scope__: We try to be careful about scope, clarifying where a variable is
   relevant and where it is not. For example, in

   ```haskell
   foo x y z =
       ..
     where
       ..
   ```

   all of `x, y, z` will be in scope in the `where` clause. If they aren't
   relevant, limit their scope:

   ```haskell
   foo x = \y z ->
       ..
     where
       ..
   ```

   this also can help to avoid awkward variable names.

   Similarly, choosing `where` over `let` can help to clarify which variables
   are in scope in those definitions. Writing

   ```haskell
   foo x = do
       ..
     where
       y = ...
   ```

   makes it very clear that the definition of `y` does not depend on anything
   that has happened within the `do` block (it depends only on the formal
   parameters of the function). The flipside of this is that `y` is then scoped
   over the entire function body; that typically is less likely to result in
   confusion (especially for local function definitions), but if it is useful to
   emphasise that `y` is only used in a small part of the function body, or that
   this avoids awkward naming, then feel free to use `let` to express that. Use
   your judgement: use scope wisely.

2. __Tuples__: We generally prefer records over tuples with lots of arguments;
   positional arguments (in tuples or as arguments to constructors) provide
   less clues what the arguments are used for.

3. __Orphans__: Orphans are generally considered bad practice, but unfortunately
   avoiding orphans at all cost often means being unable to split a module into
   smaller parts. The reason orphans are considered bad practice is that they
   might lead to incoherence; we prefer the ability to split modules into
   smaller parts and accept the loss of the help of the compiler to avoid
   incoherence as an acceptable compromise.

   Orphans in test suites are also acceptable.

4. __Assertions__: If it helps to explain what a function does, we try to be
   clear about preconditions, postconditions, and invariants. When possible, it
   is useful to reinforce such invariants with assertions so that if our
   reasoning turns out to be invalid, we will notice. The use of
   `Ouroboros.Consensus.Util.Assert.assertWithMsg` is preferred over `assert`,
   so that if the assertion fails, we get some kind of informative error message
   rather than just a Prolog-like "no".

5. __Test packages__: In order to make test code from test suite A available
   in test suite B, we define test suites as a test suite library which is
   then used by a test suite executable; the test suite library can then
   be reused by other test suites as well.

   When there are multiple test suites within a single package, it is possible
   to share some code between them by making the same source code directory
   available to all of them. However, doing so would make it impossible to
   use that shared code in a test suite defined in another package. To avoid
   this problem, we avoid sharing source directories in `cabal` files.

[fourmolu-config]: https://github.com/IntersectMBO/ouroboros-consensus/blob/master/fourmolu.yaml
