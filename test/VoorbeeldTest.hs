import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec
import Text.Parsec.String (Parser)
import ParserModule
import TypeModule
import ParserModule (pListOf)
import ParserModule (pLayout)
import GameModule
import GameModule (canMoveGame)
import GameModule (hasNextLevel)
import TypeModule (UseTime(Infinite))
import PlayerModule (inventoryContains, inventoryFull)

testGame :: Game
testGame = Game {player = Player {hp = 50, inventory = [Item {itemId = "dagger", itemX = 0, itemY = 0, itemName = "Dolk", itemDescription = "Basis schade tegen monsters", itemUseTimes = Infinite, itemValue = 10, itemActions = []}]}, levels = [Level {layout = ["********","*......*","*.....e*","*s.....*","*......*","********"], items = [Item {itemId = "sword", itemX = 2, itemY = 3, itemName = "Zwaard", itemDescription = "Meer schade tegen monsters", itemUseTimes = Infinite, itemValue = 25, itemActions = [Action {conditions = [Function {fName = "not", arguments = ArgFunction (Function {fName = "inventoryFull", arguments = Ids []})}], action = Function {fName = "retrieveItem", arguments = Ids ["sword"]}},Action {conditions = [], action = Function {fName = "leave", arguments = Ids []}}]},Item {itemId = "potion", itemX = 3, itemY = 1, itemName = "Levensbrouwsel", itemDescription = "Geeft een aantal levenspunten terug", itemUseTimes = Finite 1, itemValue = 50, itemActions = [Action {conditions = [Function {fName = "not", arguments = ArgFunction (Function {fName = "inventoryFull", arguments = Ids []})}], action = Function {fName = "retrieveItem", arguments = Ids ["potion"]}},Action {conditions = [], action = Function {fName = "leave", arguments = Ids []}}]}], entities = [Entity {entityId = "devil", entityX = 4, entityY = 3, entityName = "Duivel", entityDescription = "Een monster uit de hel", entityDirection = Nothing, entityHp = Just 50, entityValue = Just 5, entityActions = [Action {conditions = [Function {fName = "inventoryContains", arguments = Ids ["potion"]}], action = Function {fName = "increasePlayerHp", arguments = Ids ["potion"]}},Action {conditions = [Function {fName = "inventoryContains", arguments = Ids ["sword"]}], action = Function {fName = "decreaseHp", arguments = Ids ["devil","sword"]}},Action {conditions = [], action = Function {fName = "decreaseHp", arguments = Ids ["devil","dagger"]}},Action {conditions = [], action = Function {fName = "leave", arguments = Ids []}}]}]}], backup = Nothing, panelMode = PanelMode {status = Off, selectorPos = 0, panelActions = [], actionEntity = Nothing}}

main :: IO ()
main = hspec $ do

    -- ------------------------------------------------------------------------
    --                               PARSER TESTEN
    -- ------------------------------------------------------------------------

    it "Can Parse the id key-value pair" $ do
        parse (pField pString "id") "" "id: \"mijnId\"" `shouldParse` "mijnId"

    it "Can parse a function with id arguments" $ do
        parse pFunction "" "myfunc(id1, id2, id3)" `shouldParse` (Function "myfunc" (Ids ["id1", "id2", "id3"]))

    it "Can parse a function within a function with id arguments" $ do
        parse pFunction "" "myOuter(myInner(id1, id2))" `shouldParse` (Function "myOuter" (ArgFunction (Function "myInner" (Ids ["id1", "id2"]))))

    it "Can parse a conditional action" $ do
        (parse pAction) "" "[inventoryContains(potion)] increasePlayerHp(potion)"  
        `shouldParse` Action {
            conditions = [Function {fName = "inventoryContains", arguments = Ids ["potion"]}], 
            action = Function {fName = "increasePlayerHp", arguments = Ids ["potion"]}
        }

    it "Can Parse an Int" $ do
        (parse pNumber) "" "123456789" `shouldParse` 123456789

    it "Can Parse a list" $ do
        (parse (pListOf pNumber)) "" "[1, 2, 3,   4]" `shouldParse` [1, 2, 3, 4]

    it "Can Parse Infinite usetimes" $ do
        (parse pUseTime) "" "infinite" `shouldParse` Infinite

    it "Can parse finite unsetimes" $ do
        (parse pUseTime) "" "5" `shouldParse` (Finite 5)

    it "Can parse finite unsetimes" $ do
        (parse pUseTime) "" "5" `shouldParse` (Finite 5)


    it "Can Parse a layout" $ do 
        (parse pLayout) "" "\n | * * * *\n | * s e .\n | * * * *\n " `shouldParse` [['*', '*', '*', '*'], ['*', 's', 'e', '.'], ['*', '*', '*', '*']]

    -- ------------------------------------------------------------------------
    --                               SEMANTISCHE TESTEN
    -- ------------------------------------------------------------------------

    it "Speler kan niet op een muur bewegen." $ do
        canMoveGame L testGame `shouldBe` False

    it "Only one level in testGame" $ do
        hasNextLevel testGame `shouldBe` False

    it "Level has a potion" $ do
        (getName (getItem "potion" testGame))  `shouldBe` "Levensbrouwsel"


    it "Player has a dagger in inventory" $ do
        inventoryContains "dagger" (player testGame) `shouldBe` True

    it "Player does not have a sword" $ do
        inventoryContains "sword" (player testGame) `shouldBe` False

    it "Enventory of player is not full at start" $ do
        inventoryFull (player testGame)  `shouldBe` False


    it "Level has a potion" $ do
        (getName (getItem "potion" testGame))  `shouldBe` "Levensbrouwsel"

















