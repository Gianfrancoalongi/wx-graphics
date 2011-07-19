
-include_lib("wx/include/wx.hrl").

-record(frame,{bitmap :: #wx{},
	       x_paint_offset :: integer(),
	       y_paint_offset :: integer(),
	       x_move :: integer(),
	       y_move :: integer()
	      }).

-record(animation,
	{frames :: [#wx{}],
	 frame_pauses :: integer(),
	 offset_x_per_frame :: integer(),
	 offset_y_per_frame :: integer()
	 }).
