module ActionPanelModule where 

import TypeModule
import TypeModule (PanelMode, Function (Function), defaultPanel)

-- | Zet het actie paneel aan met een lijst van acties en misschien 
-- | een entity waarop de actie word uitgevoerd
togglePanelModeOn :: [ConditionalAction] ->  Maybe Entity -> PanelMode -> PanelMode
togglePanelModeOn actions mEntity = const $ PanelMode On 0 actions mEntity

-- | sluit het paneel terug af
togglePanelModeOff :: PanelMode -> PanelMode
togglePanelModeOff = const defaultPanel

-- | Pas een functie toe op de selector positie van het actie paneel 
onActionSelectorPos :: (Int -> Int) -> PanelMode -> PanelMode
onActionSelectorPos f pm = pm{selectorPos = ((`mod` length (panelActions pm)) . f) (selectorPos pm)}

-- | Move de selector in het actie paneel.
moveActionSelector :: Dir -> PanelMode -> PanelMode
moveActionSelector U = onActionSelectorPos (subtract 1)
moveActionSelector D = onActionSelectorPos (+ 1)
moveActionSelector _ = error "Cannot move selector left or right"
