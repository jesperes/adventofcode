
-define(TEST_INPUT, <<"HOH">>).
-define(TEST_INPUT_RULES, [{<<"H">>, <<"HO">>},
			   {<<"H">>, <<"OH">>},
			   {<<"O">>, <<"HH">>}]).

-define(TEST_PART2, <<"HOHOHO">>).
-define(TEST_INPUT_RULES_PART2, 
	[
	 {<<"e">>, <<"H">>},
	 {<<"e">>, <<"O">>},
	 {<<"H">>, <<"HO">>},
	 {<<"H">>, <<"OH">>},
	 {<<"O">>, <<"HH">>}
	]).

-define(INPUT, <<"ORnPBPMgArCaCaCaSiThCaCaSiThCaCaPBSiRnFArRnFArCaCaSiThCaCaSiThCaCaCaCaCaCaSiRnFYFArSiRnMgArCaSiRnPTiTiBFYPBFArSiRnCaSiRnTiRnFArSiAlArPTiBPTiRnCaSiAlArCaPTiTiBPMgYFArPTiRnFArSiRnCaCaFArRnCaFArCaSiRnSiRnMgArFYCaSiRnMgArCaCaSiThPRnFArPBCaSiRnMgArCaCaSiThCaSiRnTiMgArFArSiThSiThCaCaSiRnMgArCaCaSiRnFArTiBPTiRnCaSiAlArCaPTiRnFArPBPBCaCaSiThCaPBSiThPRnFArSiThCaSiThCaSiThCaPTiBSiRnFYFArCaCaPRnFArPBCaCaPBSiRnTiRnFArCaPRnFArSiRnCaCaCaSiThCaRnCaFArYCaSiRnFArBCaCaCaSiThFArPBFArCaSiRnFArRnCaCaCaFArSiRnFArTiRnPMgArF">>).

-define(INPUT_RULES, 
	[
	 {<<"Al">>, <<"ThF">>},
	 {<<"Al">>, <<"ThRnFAr">>},
	 {<<"B">>, <<"BCa">>},
	 {<<"B">>, <<"TiB">>},
	 {<<"B">>, <<"TiRnFAr">>},
	 {<<"Ca">>, <<"CaCa">>},
	 {<<"Ca">>, <<"PB">>},
	 {<<"Ca">>, <<"PRnFAr">>},
	 {<<"Ca">>, <<"SiRnFYFAr">>},
	 {<<"Ca">>, <<"SiRnMgAr">>},
	 {<<"Ca">>, <<"SiTh">>},
	 {<<"F">>, <<"CaF">>},
	 {<<"F">>, <<"PMg">>},
	 {<<"F">>, <<"SiAl">>},
	 {<<"H">>, <<"CRnAlAr">>},
	 {<<"H">>, <<"CRnFYFYFAr">>},
	 {<<"H">>, <<"CRnFYMgAr">>},
	 {<<"H">>, <<"CRnMgYFAr">>},
	 {<<"H">>, <<"HCa">>},
	 {<<"H">>, <<"NRnFYFAr">>},
	 {<<"H">>, <<"NRnMgAr">>},
	 {<<"H">>, <<"NTh">>},
	 {<<"H">>, <<"OB">>},
	 {<<"H">>, <<"ORnFAr">>},
	 {<<"Mg">>, <<"BF">>},
	 {<<"Mg">>, <<"TiMg">>},
	 {<<"N">>, <<"CRnFAr">>},
	 {<<"N">>, <<"HSi">>},
	 {<<"O">>, <<"CRnFYFAr">>},
	 {<<"O">>, <<"CRnMgAr">>},
	 {<<"O">>, <<"HP">>},
	 {<<"O">>, <<"NRnFAr">>},
	 {<<"O">>, <<"OTi">>},
	 {<<"P">>, <<"CaP">>},
	 {<<"P">>, <<"PTi">>},
	 {<<"P">>, <<"SiRnFAr">>},
	 {<<"Si">>, <<"CaSi">>},
	 {<<"Th">>, <<"ThCa">>},
	 {<<"Ti">>, <<"BP">>},
	 {<<"Ti">>, <<"TiTi">>},
	 {<<"e">>, <<"HF">>},
	 {<<"e">>, <<"NAl">>},
	 {<<"e">>, <<"OMg">>}
	]).


