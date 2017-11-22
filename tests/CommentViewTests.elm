module CommentViewTests exposing (commentViewTests)

import CommentModel
    exposing
        ( UserCommentModel
        , UserIdModel(UserIdModel)
        , commentModelInit
        , commentZipperInit
        )
import CommentStyles exposing (stylesheet)
import CommentView exposing (..)
import Element exposing (viewport)
import Expect
import Fuzz
    exposing
        ( bool
        )
import Test
    exposing
        ( Test
        , describe
        , fuzz
        , test
        )
import Test.Html.Query
    exposing
        ( count
        , find
        , findAll
        , fromHtml
        , has
        )
import Test.Html.Selector
    exposing
        ( attribute
        , tag
        )


commenter : UserCommentModel
commenter =
    UserCommentModel (UserIdModel "") "" ""


commentViewTests : Test
commentViewTests =
    describe "CommentView Tests"
        [ test "commentProfileHtml Html Test" <|
            \() ->
                commentProfileHtml commenter
                    |> viewport (stylesheet "")
                    |> fromHtml
                    |> findAll [ tag "a" ]
                    |> count (Expect.equal 1)
        , fuzz bool "addCommentView Html Test: button should be disabled even when user signed because the textarea will be empty" <|
            \isSignedIn ->
                addCommentView isSignedIn commenter commentZipperInit commentModelInit.commentId
                    |> viewport (stylesheet "")
                    |> fromHtml
                    |> find [ tag "button" ]
                    |> has [ attribute "disabled" "" ]
        , test "commentMainView Html Test" <|
            \() ->
                commentMainView True commenter commentZipperInit
                    |> viewport (stylesheet "")
                    |> fromHtml
                    |> findAll [ tag "button" ]
                    |> count (Expect.equal 1)
        , test "commentMainViewTextArea Html Test" <|
            \() ->
                commentMainView True commenter commentZipperInit
                    |> viewport (stylesheet "")
                    |> fromHtml
                    |> findAll [ tag "textarea" ]
                    |> count (Expect.equal 1)
        ]
