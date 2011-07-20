
-include_lib("wx/include/wx.hrl").

-record(frame,{bitmap :: #wx{},
	       x_paint_offset :: integer(),
	       y_paint_offset :: integer(),
	       x_move :: integer(),
	       y_move :: integer(),
	       frame_pause :: integer()
	      }).

-record(animation,
	{frames :: [#frame{}]
	}).

-type point() :: {integer(),integer()}.
