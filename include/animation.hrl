-record(animation,
	{frames :: [term()], %% [wx{}]
	 frame_pauses :: integer(),
	 offset_x_per_frame :: integer(),
	 offset_y_per_frame :: integer()
	 }).
