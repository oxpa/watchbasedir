-record(rpm_lead,{
        magic,
        major,
        minor,
        type,
        archnum,
        name,
        osnum,
        signature_type,
        reserved}
).
-record(rpm_tag_index, {tag, data_type, offset, num_of_entries}).
-record(rpm, {filename, lead, signature, header, fileprops, checksum}).

rpm_types() ->
	get_rpmTagType_e().
%  [{0,null},
%  {1,char},
%  {2,int8},
%  {3,int16},
%  {4,int32},
%  {5,int64},
%  {6,string},
%  {7,bin}, %size in bytes
%  {8,string_array},
%  {9,i18n_string}
%  ].
rpm_signature_tags() ->
	get_rpmSigTag_e().
	
rpm_tags() ->
	get_rpmTag_e().

rpm_types_sizes() ->
        [{0,0},
        {1,1},
        {2,1},
        {3,2},
        {4,4},
        {5,8},
        {6,0}, % not appliciable
        {7,1},
        {8,0}, % not appliciable
        {9,0} %
        ].

%% Following text is a data parsed from rpmtags.h 
%% it might be inaccurate in any possible way

%#define	header_image		61
%#define	header_signatures	62
%#define	header_immutable	63
%#define	header_regions		64
%#define header_i18ntable	100
%#define	header_sigbase		256
%#define	header_tagbase		1000

get_rpmTag_e() -> [ 
{ -1,		rpmtag_not_found		},			
{ 61,		rpmtag_headerimage		},		
{ 62,		rpmtag_headersignatures	},	
{ 63,		rpmtag_headerimmutable	},	
{ 64,		rpmtag_headerregions	},	
{ 100,		rpmtag_headeri18ntable	}, 	
{ 256,		rpmtag_sig_base			},
{ 256+1,	rpmtag_sigsize			},	
{ 256+2,	rpmtag_siglemd5_1		},	
{ 256+3,	rpmtag_sigpgp			},	
{ 256+4,	rpmtag_siglemd5_2		},	
{ 256+5,	rpmtag_sigmd5	        },	
{ 256+5,	rpmtag_pkgid	        },
{ 256+6,	rpmtag_siggpg	        },	
{ 256+7,	rpmtag_sigpgp5	        },	
{ 256+8,	rpmtag_badsha1_1		},	
{ 256+9,	rpmtag_badsha1_2		},	
{ 256+10,	rpmtag_pubkeys			},	
{ 256+11,   rpmtag_dsaheader		},	
{ 256+12,   rpmtag_rsaheader		},	
{ 256+13,   rpmtag_sha1header		},	
{ 256+13,	rpmtag_hdrid			},
{ 256+14,   rpmtag_longsigsize		},	
{ 256+15,   rpmtag_longarchivesize	},	
{ 1000,		rpmtag_name				},	
{ 1001,   	rpmtag_version			},	
{ 1002,   	rpmtag_release			},	
{ 1003,   	rpmtag_epoch   		},	
{ 1004,   	rpmtag_summary		},	
{ 1005,   	rpmtag_description		},	
{ 1006,   	rpmtag_buildtime		},	
{ 1007,   	rpmtag_buildhost		},	
{ 1008,   	rpmtag_installtime		},	
{ 1009,   	rpmtag_size			},	
{ 1010,   	rpmtag_distribution		},	
{ 1011,   	rpmtag_vendor		},	
{ 1012,   	rpmtag_gif			},	
{ 1013,   	rpmtag_xpm			},	
{ 1014,   	rpmtag_license		},	
{ 1015,   	rpmtag_packager		},	
{ 1016,   	rpmtag_group		},	
{ 1017,   	rpmtag_changelog		}, 
{ 1018,   	rpmtag_source		},	
{ 1019,   	rpmtag_patch		},	
{ 1020,   	rpmtag_url			},	
{ 1021,   	rpmtag_os			},	
{ 1022,   	rpmtag_arch			},	
{ 1023,   	rpmtag_prein		},	
{ 1024,   	rpmtag_postin		},	
{ 1025,   	rpmtag_preun		},	
{ 1026,   	rpmtag_postun		},	
{ 1027,   	rpmtag_oldfilenames		}, 
{ 1028,   	rpmtag_filesizes		},	
{ 1029,   	rpmtag_filestates		}, 
{ 1030,   	rpmtag_filemodes		},	
{ 1031,   	rpmtag_fileuids		}, 
{ 1032,   	rpmtag_filegids		}, 
{ 1033,   	rpmtag_filerdevs		},	
{ 1034,   	rpmtag_filemtimes		}, 
{ 1035,   	rpmtag_filedigests		},	
{ 1035,		rpmtag_filemd5s			},
{ 1036,		rpmtag_filelinktos		},	
{ 1037,   	rpmtag_fileflags		},	
{ 1038,   	rpmtag_root			}, 
{ 1039,   	rpmtag_fileusername		},	
{ 1040,   	rpmtag_filegroupname	},	
{ 1041,   	rpmtag_exclude		}, 
{ 1042,   	rpmtag_exclusive		}, 
{ 1043,   	rpmtag_icon			}, 
{ 1044,   	rpmtag_sourcerpm		},	
{ 1045,   	rpmtag_fileverifyflags	},	
{ 1046,   	rpmtag_archivesize		},	
{ 1047,   	rpmtag_providename		},	
{ 1048,   	rpmtag_requireflags		},	
{ 1049,   	rpmtag_requirename		},	
{ 1050,   	rpmtag_requireversion	},	
{ 1051,   	rpmtag_nosource		}, 
{ 1052,   	rpmtag_nopatch		}, 
{ 1053,   	rpmtag_conflictflags	}, 
{ 1054,   	rpmtag_conflictname		},	
{ 1055,   	rpmtag_conflictversion	},	
{ 1056,   	rpmtag_defaultprefix	}, 
{ 1057,   	rpmtag_buildroot		}, 
{ 1058,   	rpmtag_installprefix	}, 
{ 1059,   	rpmtag_excludearch		}, 
{ 1060,   	rpmtag_excludeos		}, 
{ 1061,   	rpmtag_exclusivearch	}, 
{ 1062,   	rpmtag_exclusiveos		}, 
{ 1063,   	rpmtag_autoreqprov		}, 
{ 1064,   	rpmtag_rpmversion		},	
{ 1065,   	rpmtag_triggerscripts	},	
{ 1066,   	rpmtag_triggername		},	
{ 1067,   	rpmtag_triggerversion	},	
{ 1068,   	rpmtag_triggerflags		},	
{ 1069,   	rpmtag_triggerindex		},	
{ 1079,   	rpmtag_verifyscript		},	
{ 1080,   	rpmtag_changelogtime	},	
{ 1081,   	rpmtag_changelogname	},	
{ 1082,   	rpmtag_changelogtext	},	
{ 1083,   	rpmtag_brokenmd5		}, 
{ 1084,   	rpmtag_prereq		}, 
{ 1085,   	rpmtag_preinprog		},	
{ 1086,   	rpmtag_postinprog		},	
{ 1087,   	rpmtag_preunprog		},	
{ 1088,   	rpmtag_postunprog		},	
{ 1089,   	rpmtag_buildarchs		}, 
{ 1090,   	rpmtag_obsoletename		},	
{ 1091,   	rpmtag_verifyscriptprog	},	
{ 1092,   	rpmtag_triggerscriptprog	},	
{ 1093,   	rpmtag_docdir		}, 
{ 1094,   	rpmtag_cookie		},	
{ 1095,   	rpmtag_filedevices		},	
{ 1096,   	rpmtag_fileinodes		},	
{ 1097,   	rpmtag_filelangs		},	
{ 1098,   	rpmtag_prefixes		},	
{ 1099,   	rpmtag_instprefixes		},	
{ 1100,   	rpmtag_triggerin		}, 
{ 1101,   	rpmtag_triggerun		}, 
{ 1102,   	rpmtag_triggerpostun	}, 
{ 1103,   	rpmtag_autoreq		}, 
{ 1104,   	rpmtag_autoprov		}, 
{ 1105,   	rpmtag_capability		}, 
{ 1106,   	rpmtag_sourcepackage	}, 
{ 1107,   	rpmtag_oldorigfilenames	}, 
{ 1108,   	rpmtag_buildprereq		}, 
{ 1109,   	rpmtag_buildrequires	}, 
{ 1110,   	rpmtag_buildconflicts	}, 
{ 1111,   	rpmtag_buildmacros		}, 
{ 1112,   	rpmtag_provideflags		},	
{ 1113,   	rpmtag_provideversion	},	
{ 1114,   	rpmtag_obsoleteflags	},	
{ 1115,   	rpmtag_obsoleteversion	},	
{ 1116,   	rpmtag_dirindexes		},	
{ 1117,   	rpmtag_basenames		},	
{ 1118,   	rpmtag_dirnames		},	
{ 1119,   	rpmtag_origdirindexes	}, 
{ 1120,   	rpmtag_origbasenames	}, 
{ 1121,   	rpmtag_origdirnames		}, 
{ 1122,   	rpmtag_optflags		},	
{ 1123,   	rpmtag_disturl		},	
{ 1124,   	rpmtag_payloadformat	},	
{ 1125,   	rpmtag_payloadcompressor	},	
{ 1126,   	rpmtag_payloadflags		},	
{ 1127,   	rpmtag_installcolor		}, 
{ 1128,   	rpmtag_installtid		},	
{ 1129,   	rpmtag_removetid		},	
{ 1130,   	rpmtag_sha1rhn		}, 
{ 1131,   	rpmtag_rhnplatform		},	
{ 1132,   	rpmtag_platform		},	
{ 1133,   	rpmtag_patchesname		}, 
{ 1134,   	rpmtag_patchesflags		}, 
{ 1135,   	rpmtag_patchesversion	}, 
{ 1136,   	rpmtag_cachectime		},	
{ 1137,   	rpmtag_cachepkgpath		},	
{ 1138,   	rpmtag_cachepkgsize		},	
{ 1139,   	rpmtag_cachepkgmtime	},	
{ 1140,   	rpmtag_filecolors		},	
{ 1141,   	rpmtag_fileclass		},	
{ 1142,   	rpmtag_classdict		},	
{ 1143,   	rpmtag_filedependsx		},	
{ 1144,   	rpmtag_filedependsn		},	
{ 1145,   	rpmtag_dependsdict		},	
{ 1146,   	rpmtag_sourcepkgid		},	
{ 1147,   	rpmtag_filecontexts		},	
{ 1148,   	rpmtag_fscontexts		},	
{ 1149,   	rpmtag_recontexts		},	
{ 1150,   	rpmtag_policies		},	
{ 1151,   	rpmtag_pretrans		},	
{ 1152,   	rpmtag_posttrans		},	
{ 1153,   	rpmtag_pretransprog		},	
{ 1154,   	rpmtag_posttransprog	},	
{ 1155,   	rpmtag_disttag		},	
{ 1156,   	rpmtag_suggestsname		},	
{ 1157,   	rpmtag_suggestsversion	},	
{ 1158,   	rpmtag_suggestsflags	},	
{ 1159,   	rpmtag_enhancesname		},	
{ 1160,   	rpmtag_enhancesversion	},	
{ 1161,   	rpmtag_enhancesflags	},	
{ 1162,   	rpmtag_priority		}, 
{ 1163,   	rpmtag_cvsid		}, 
{ 1164,   	rpmtag_blinkpkgid		}, 
{ 1165,   	rpmtag_blinkhdrid		}, 
{ 1166,   	rpmtag_blinknevra		}, 
{ 1167,   	rpmtag_flinkpkgid		}, 
{ 1168,   	rpmtag_flinkhdrid		}, 
{ 1169,   	rpmtag_flinknevra		}, 
{ 1170,   	rpmtag_packageorigin	}, 
{ 1171,   	rpmtag_triggerprein		}, 
{ 1172,   	rpmtag_buildsuggests	}, 
{ 1173,   	rpmtag_buildenhances	}, 
{ 1174,   	rpmtag_scriptstates		}, 
{ 1175,   	rpmtag_scriptmetrics	}, 
{ 1176,   	rpmtag_buildcpuclock	}, 
{ 1177,   	rpmtag_filedigestalgos	}, 
{ 1178,   	rpmtag_variants		}, 
{ 1179,   	rpmtag_xmajor		}, 
{ 1180,   	rpmtag_xminor		}, 
{ 1181,   	rpmtag_repotag		},	
{ 1182,   	rpmtag_keywords		},	
{ 1183,   	rpmtag_buildplatforms	},	
{ 1184,   	rpmtag_packagecolor		}, 
{ 1185,   	rpmtag_packageprefcolor	}, 
{ 1186,   	rpmtag_xattrsdict		}, 
{ 1187,   	rpmtag_filexattrsx		}, 
{ 1188,   	rpmtag_depattrsdict		}, 
{ 1189,   	rpmtag_conflictattrsx	}, 
{ 1190,   	rpmtag_obsoleteattrsx	}, 
{ 1191,   	rpmtag_provideattrsx	}, 
{ 1192,   	rpmtag_requireattrsx	}, 
{ 1193,   	rpmtag_buildprovides	}, 
{ 1194,   	rpmtag_buildobsoletes	}, 
{ 1195,   	rpmtag_dbinstance		}, 
{ 1196,   	rpmtag_nvra			}, 
{ 5000,   	rpmtag_filenames		}, 
{ 5001,   	rpmtag_fileprovide		}, 
{ 5002,   	rpmtag_filerequire		}, 
{ 5003,   	rpmtag_fsnames		}, 
{ 5004,   	rpmtag_fssizes		}, 
{ 5005,   	rpmtag_triggerconds		}, 
{ 5006,   	rpmtag_triggertype		}, 
{ 5007,   	rpmtag_origfilenames	}, 
{ 5008,   	rpmtag_longfilesizes	},	
{ 5009,   	rpmtag_longsize		}, 
{ 5010,   	rpmtag_filecaps		}, 
{ 5011,   	rpmtag_filedigestalgo	}, 
{ 5012,   	rpmtag_bugurl		}, 
{ 5013,   	rpmtag_evr			}, 
{ 5014,   	rpmtag_nvr			}, 
{ 5015,   	rpmtag_nevr			}, 
{ 5016,   	rpmtag_nevra		}, 
{ 5017,   	rpmtag_headercolor		}, 
{ 5018,   	rpmtag_verbose		}, 
{ 5019,   	rpmtag_epochnum		}, 
{ 5020,   	rpmtag_preinflags		}, 
{ 5021,   	rpmtag_postinflags		}, 
{ 5022,   	rpmtag_preunflags		}, 
{ 5023,   	rpmtag_postunflags		}, 
{ 5024,   	rpmtag_pretransflags	}, 
{ 5025,   	rpmtag_posttransflags	}, 
{ 5026,   	rpmtag_verifyscriptflags	}, 
{ 5027,   	rpmtag_triggerscriptflags	}, 
{ 5029,   	rpmtag_collections		}, 
{ 5030,   	rpmtag_policynames		},	
{ 5031,   	rpmtag_policytypes		},	
{ 5032,   	rpmtag_policytypesindexes	},	
{ 5033,   	rpmtag_policyflags		},	
{ 5034,   	rpmtag_vcs			}, 
{ 5035,   	rpmtag_ordername		},	
{ 5036,   	rpmtag_orderversion		},	
{ 5037,   	rpmtag_orderflags		}	
].
%% #define	RPMTAG_EXTERNAL_TAG		1000000

get_rpmDbiTag_e() -> [ 
{ 0,   rpmdbi_packages			},	
{ 2,   rpmdbi_label			},	
{ rpmtag_name,   rpmdbi_name				},
{ rpmtag_basenames,   rpmdbi_basenames		},
{ rpmtag_group,   rpmdbi_group			},
{ rpmtag_requirename,   rpmdbi_requirename		},
{ rpmtag_providename,   rpmdbi_providename		},
{ rpmtag_conflictname,   rpmdbi_conflictname		},
{ rpmtag_obsoletename,   rpmdbi_obsoletename		},
{ rpmtag_triggername,   rpmdbi_triggername		},
{ rpmtag_dirnames,   rpmdbi_dirnames			},
{ rpmtag_installtid,   rpmdbi_installtid		},
{ rpmtag_sigmd5,   rpmdbi_sigmd5			},
{ rpmtag_sha1header,   rpmdbi_sha1header		}
].
get_rpmSigTag_e() -> [ 
{ 1000,   rpmsigtag_size		},	
{ 1001,   rpmsigtag_lemd5_1	},	
{ 1002,   rpmsigtag_pgp		},	
{ 1003,   rpmsigtag_lemd5_2	},	
{ 1004,   rpmsigtag_md5		},	
{ 1005,   rpmsigtag_gpg		}, 
{ 1006,   rpmsigtag_pgp5		},	
{ 1007,   rpmsigtag_payloadsize },
{ 264,   rpmsigtag_badsha1_1	},	
{ 265,   rpmsigtag_badsha1_2	},	
{ 269,   rpmsigtag_sha1		},	
{ 267,   rpmsigtag_dsa		},	
{ 268,   rpmsigtag_rsa		},	
{ 270,   rpmsigtag_longsize	},	
{ 271,   rpmsigtag_longarchivesize } 
].
get_rpmTagType_e() -> [ 
{  0,   rpm_null_type		},
{  1,   rpm_char_type		},
{  2,   rpm_int8_type		},
{  3,   rpm_int16_type		},
{  4,   rpm_int32_type		},
{  5,   rpm_int64_type		},
{  6,   rpm_string_type		},
{  7,   rpm_bin_type		},
{  8,   rpm_string_array_type	},
{  9,   rpm_i18nstring_type		}
].
get_rpmTagClass_e() -> [ 
{ 0,   rpm_null_class	},
{ 1,   rpm_numeric_class	},
{ 2,   rpm_string_class	},
{ 3,   rpm_binary_class	}].

get_rpmSubTagType_e() -> [ 
{ -10,   rpm_region_type		},
{ -11,   rpm_bin_array_type		},
{ -12,   rpm_xref_type		}
].
%rpmTagReturnType_e() -> [
%{   rpm_any_return_type         , 0},
%{   rpm_scalar_return_type      , 0x00010000},
%{   rpm_array_return_type       , 0x00020000},
%{   rpm_mapping_return_type     , 0x00040000},
%{   rpm_mask_return_type        , 0xffff0000}].
