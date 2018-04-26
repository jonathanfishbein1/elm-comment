module CommentViewTests exposing (commentViewTests)

import CommentModel
    exposing
        ( UserCommentModel
        , UserIdModel(UserIdModel)
        , commentModelInit
        , commentZipperInit
        )
import CommentView exposing (commentMainView)
import Element
import Expect
import Fuzz
    exposing
        ( bool
        )
import Internal.CommentView
    exposing
        ( addCommentView
        , commentProfileHtml
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
                    |> Element.layout []
                    |> fromHtml
                    |> findAll [ tag "a" ]
                    |> count (Expect.equal 1)
        , fuzz bool "addCommentView Html Test: button should be disabled even when user signed because the textarea will be empty" <|
            \isSignedIn ->
                addCommentView isSignedIn commenter commentZipperInit commentModelInit.commentId
                    |> Element.layout []
                    |> fromHtml
                    |> find [ tag "button" ]
                    |> has [ attribute "disabled" "" ]
        , test "commentMainView Html Test" <|
            \() ->
                commentMainView True commenter commentZipperInit
                    |> Element.layout []
                    |> fromHtml
                    |> findAll [ tag "button" ]
                    |> count (Expect.equal 1)
        , test "commentMainViewTextArea Html Test" <|
            \() ->
                commentMainView True commenter commentZipperInit
                    |> Element.layout []
                    |> fromHtml
                    |> findAll [ tag "textarea" ]
                    |> count (Expect.equal 1)
        ]
