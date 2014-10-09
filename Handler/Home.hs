{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import


homeTitle :: MonadWidget m => m ()
homeTitle = setTitle "Akateeminen saunakerho Löyly ry"


getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        homeTitle
        $(widgetFile "homepage")

getMembersR, postMembersR :: Handler Html
getMembersR  = postMembersR
postMembersR = do
    ((result, formWidget), formEnctype) <- runFormPost memberForm

    case result of
        FormSuccess res -> do _ <- runDB $ insert res
                              return ()
        _ -> return ()

    defaultLayout $ do
        setTitle "Jäsenet ja liittyminen"
        $(widgetFile "members")

getActivitiesR :: Handler Html
getActivitiesR = do
    defaultLayout $ do
        setTitle "Toiminta"
        $(widgetFile "activities")

memberForm :: Form Member
memberForm = renderDivs $ Member
    <$> areq textField "Koko nimi" Nothing
    <*> areq textField "Kotikunta" Nothing
    <*> areq emailField "Sähköposti" Nothing
    <*> areq dayField "Syntymäaika" Nothing
    <*> areq boolField "Sukupuoli" (Just True)

