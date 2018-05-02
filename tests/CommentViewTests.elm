module CommentViewTests exposing (commentViewTests)

import CommentModel
    exposing
        ( UserCommentModel
        , UserIdModel(UserIdModel)
        , commentZipperInit
        )
import CommentView exposing (commentMainView)
import Element
import Expect
import Fuzz
import Internal.CommentView exposing (commentProfileHtml)
import Test
    exposing
        ( Test
        , describe
        , test
        )
import Test.Html.Query
    exposing
        ( count
        , findAll
        , fromHtml
        )
import Test.Html.Selector exposing (tag)


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
        , test "commentMainViewTextArea Html Test" <|
            \() ->
                commentMainView True commenter commentZipperInit
                    |> Element.layout []
                    |> fromHtml
                    |> findAll [ tag "textarea" ]
                    |> count (Expect.equal 1)
        ]
