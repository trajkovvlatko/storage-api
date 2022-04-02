module Controllers.SearchSpec (spec) where

import Controllers.Search
import Factories (createDrawer, createItem, createRoom, createStorageUnit, createUser)
import Helpers (getToken, loginUser, shouldContainString)
import Models.Drawer (Drawer (Drawer, dLevel, dNote))
import Models.Room (Room (Room, rName))
import Models.StorageUnit (StorageUnit (StorageUnit, sName))
import Server (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

spec :: Spec
spec = with app $ do
  describe "index" $ do
    context "without authenticated user" $ do
      it "returns an error for missing token" $ do
        userId <- liftIO $ createUser "user@user.com" "password"
        liftIO $ createItem userId "black" "electronics" "laptop" Nothing

        let response = get "/search/basic/laptop"

        response `shouldRespondWith` [json|{message: "Invalid user token."}|] {matchStatus = 401}

  context "with authenticated user" $ do
    it "returns an empty list for missing parameter" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      liftIO $ createItem userId "black" "electronics" "laptop" Nothing
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/" [("token", getToken loginResponse)] ""

      response `shouldRespondWith` [json|[]|] {matchStatus = 200}

    it "returns an empty list for no item found" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      liftIO $ createItem userId "black" "electronics" "laptop" Nothing
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/other" [("token", getToken loginResponse)] ""

      response `shouldRespondWith` [json|[]|] {matchStatus = 200}

    it "returns items created by the current user" $ do
      userId <- liftIO $ createUser "user@user.com" "password"
      otherUserId <- liftIO $ createUser "other@other.com" "other"
      (roomId, roomName) <- liftIO $ createRoom userId "room 123"
      let room = Room roomId userId roomName
      (storageUnitId, _, storageUnitName) <- liftIO $ createStorageUnit userId "storage unit name 123" (Just room)
      let storageUnit = StorageUnit storageUnitId userId roomId storageUnitName
      (drawerId, _, drawerLevel, drawerNote) <- liftIO $ createDrawer userId 123 "drawer note 123" (Just storageUnit)
      let drawer = Drawer drawerId userId storageUnitId drawerLevel drawerNote
      (_, _, _, _, _, itemName) <- liftIO $ createItem userId "black" "electronics" "laptop" (Just drawer)

      liftIO $ createItem otherUserId "red" "clothes" "laptop" Nothing

      loginResponse <- loginUser

      let response = request "GET" "/search/basic/laptop" [("token", getToken loginResponse)] ""

      response
        `shouldRespondWith` [json|[{
          name: #{itemName},
          drawer_note: #{drawerNote},
          drawer_level: #{drawerLevel},
          storage_unit_name: #{storageUnitName},
          room_name: #{roomName},
          color: "black",
          item_type: "electronics"
        }]|]
          { matchStatus = 200
          }

    it "returns list of items " $ do
      userId <- liftIO $ createUser "user@user.com" "password"

      (roomId1, roomName1) <- liftIO $ createRoom userId "room 123"
      let room1 = Room roomId1 userId roomName1
      (storageUnitId1, _, storageUnitName1) <- liftIO $ createStorageUnit userId "storage unit name 123" (Just room1)
      let storageUnit1 = StorageUnit storageUnitId1 userId roomId1 storageUnitName1
      (drawerId1, _, drawerLevel1, drawerNote1) <- liftIO $ createDrawer userId 123 "drawer note 123" (Just storageUnit1)
      let drawer1 = Drawer drawerId1 userId storageUnitId1 drawerLevel1 drawerNote1
      (_, _, _, _, _, itemName1) <- liftIO $ createItem userId "black" "electronics" "last item name 1" (Just drawer1)

      (roomId2, roomName2) <- liftIO $ createRoom userId "room 456"
      let room2 = Room roomId2 userId roomName2
      (storageUnitId2, _, storageUnitName2) <- liftIO $ createStorageUnit userId "storage unit name 456" (Just room2)
      let storageUnit2 = StorageUnit storageUnitId2 userId roomId2 storageUnitName2
      (drawerId2, _, drawerLevel2, drawerNote2) <- liftIO $ createDrawer userId 456 "drawer note 456" (Just storageUnit2)
      let drawer2 = Drawer drawerId2 userId storageUnitId2 drawerLevel2 drawerNote2
      (_, _, _, _, _, itemName2) <- liftIO $ createItem userId "green" "cables" "first item name 2" (Just drawer2)

      liftIO $ createItem userId "red" "clothes" "something else" Nothing
      loginResponse <- loginUser

      let response = request "GET" "/search/basic/nam" [("token", getToken loginResponse)] ""

      response
        `shouldRespondWith` [json|[{
          name: #{itemName2},
          drawer_note: #{drawerNote2},
          drawer_level: #{drawerLevel2},
          storage_unit_name: #{storageUnitName2},
          room_name: #{roomName2},
          color: "green",
          item_type: "cables"
        }, {
          name: #{itemName1},
          drawer_note: #{drawerNote1},
          drawer_level: #{drawerLevel1},
          storage_unit_name: #{storageUnitName1},
          room_name: #{roomName1},
          color: "black",
          item_type: "electronics"
        }]|]
          { matchStatus = 200
          }
