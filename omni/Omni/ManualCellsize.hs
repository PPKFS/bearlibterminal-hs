module Omni.ManualCellsize where

import Formatting
import qualified Data.Vector as V
import BearLibTerminal
import Control.Monad.State

data GuiState = GuiState
  { hinting :: Int
  , size :: Int
  , cellWidth :: Int
  , cellHeight :: Int
  }

initialGui :: GuiState
initialGui = GuiState 0 12 8 16

manualCellsize :: IO ()
manualCellsize = do
  terminalSet_ "window.title='Omni: manual cellsize'"

  let font = "Media/VeraMono.ttf"
      fontHintings = V.fromList ["normal", "autohint", "none"]
      setupCellsize = do
        s <- get
        terminalSet_ (sformat ("window: cellsize=" % int % "x" % int) (cellWidth s) (cellHeight s))

  terminalSet_ $ sformat ("font: " % stext % ", size=" % int % ", hinting=" % stext) font (size initialGui) (fontHintings V.! (hinting initialGui))
  flip (evalStateT setupCellsize
	{-
  auto setup_cellsize = [&](){terminal_setf("window: cellsize=%dx%d", cell_width, cell_height);};

	setup_cellsize();
	setup_font();

	while (true)
	{
		terminal_clear();
		terminal_color("white");

		terminal_printf(2, 1, "Hello, world!");
		terminal_printf(2, 3, "[color=orange]Font size:[/color] %d", font_size);
		terminal_printf(2, 4, "[color=orange]Font hinting:[/color] %s", font_hintings[font_hinting].c_str());
		terminal_printf(2, 5, "[color=orange]Cell size:[/color] %dx%d", cell_width, cell_height);
		terminal_printf(2, 7, "[color=orange]TIP:[/color] Use arrow keys to change cell size");
		terminal_printf(2, 8, "[color=orange]TIP:[/color] Use Shift+Up/Down arrow keys to change font size");
		terminal_printf(2, 9, "[color=orange]TIP:[/color] Use TAB to switch font hinting mode");

		terminal_refresh();

		int key = terminal_read();

		if (key == TK_CLOSE || key == TK_ESCAPE)
		{
			break;
		}
		else if (key == TK_LEFT && !terminal_state(TK_SHIFT) && cell_width > 4)
		{
			cell_width -= 1;
			setup_cellsize();
		}
		else if (key == TK_RIGHT && !terminal_state(TK_SHIFT) && cell_width < 24)
		{
			cell_width += 1;
			setup_cellsize();
		}
		else if (key == TK_DOWN && !terminal_state(TK_SHIFT) && cell_height < 24)
		{
			cell_height += 1;
			setup_cellsize();
		}
		else if (key == TK_UP && !terminal_state(TK_SHIFT) && cell_height > 4)
		{
			cell_height -= 1;
			setup_cellsize();
		}
		else if (key == TK_UP && terminal_state(TK_SHIFT) && font_size < 64)
		{
			font_size += 1;
			setup_font();
		}
		else if (key == TK_DOWN && terminal_state(TK_SHIFT) && font_size > 4)
		{
			font_size -= 1;
			setup_font();
		}
		else if (key == TK_TAB)
		{
			font_hinting = (font_hinting + 1) % font_hintings.size();
			setup_font();
		}
	}

	terminal_set("font: default; window.cellsize=auto");
  -}