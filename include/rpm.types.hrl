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
-record(rpm_sig_index, {tag, data_type, offset, num_of_entries}).

rpm_types() ->
  [{0,null},
  {1,char},
  {2,int8},
  {3,int16},
  {4,int32},
  {5,int64},
  {6,string},
  {7,bin}, %size in bytes
  {8,string_array},
  {9,i18n_string}
  ].

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




rpm_signature_tags() ->
  [
    {1000, signature_size},
    {1002, pgp_header},
    {1004, md5_header},
    {1007, signature_payloadsize}
  ].


rpm_tags() ->
  [
  {62, header_signatures},
  {63, headerimmutable},
  {100, headeri18ntable},
  {268, rsa_header},
  {269, sha1_header},
  {1000, name}, % size for signature
  {1001, version},
  {1002, release}, % pgp for signature
  {1003, epoch},
  {1004, summary}, % md5 for signature
  {1005, description},
  {1006, buildtime},
  {1007, buildhost}, % this is payloadsize for signature
  {1008, installtime},
  {1009, size},
  {1010, distribution},
  {1011, vendor},
  {1012, gif},
  {1013, xpm},
  {1014, license},
  {1015, packager},
  {1016, group},
  {1017, changelog},
  {1018, source},
  {1019, patch},
  {1020, url},
  {1021, os},
  {1022, arch},
  {1023, preinstall},
  {1024, postinstall},
  {1025, preuninstall},
  {1026, postuninstall},
  {1027, old_filenames},
  {1028, filesizes},
  {1029, filestates},
  {1030, filemodes},
  {1031, fileuids},
  {1032, filegids},
  {1033, filerdevs},
  {1034, filemtimes},
  {1035, filedigests},
  {1036, filelinktos},
  {1037, fileflags},
  {1038, root},
  {1039, fileusername},
  {1040, filegroupname},
  {1041, exclude},
  {1042, exlusive},
  {1043, icon},
  {1044, sourcerpm},
  {1045, fileverifyflags},
  {1046, archivesize},
  {1047, providename},
  {1048, requireflags},
  {1049, requirename},
  {1050, requireversion},
  {1051, nosource},
  {1052, nopatch},
  {1053, conflictflags},
  {1054, conflictname},
  {1055, conflictversion},
  {1056, defaultprefix},
  {1057, buildroot},
  {1058, installprefix},
  {1059, excludearch},
  {1060, excludeos},
  {1061, exlusivearch},
  {1062, exlusiveos},
  {1063, autoreqprov},
  {1064, rpmversion},
  {1065, triggerscripts},
  {1066, triggername},
  {1067, triggerversion},
  {1068, triggerflags},
  {1069, triggerindex},
  {1079, verifyscript},
  {1080, changelogtime},
  {1081, changelogname},
  {1082, changelogtext},
  {1085, preinstall_prog},
  {1086, postinstall_prog},
  {1087, preuninstall_prog},
  {1088, postuninstall_prog},
  {1089, buildarch},
  {1090, obsoletename},
  {1092, triggerscript_prog},
  {1093, docdir},
  {1094, cookie},
  {1095, filedevices},
  {1096, fileinodes},
  {1097, filelangs},
  {1098, prefixes},
  {1112, provideflags},
  {1113, provideversion},
  {1114, obsoleteflags},
  {1115, obsoleteversion},
  {1116, dirindexes},
  {1117, basenames},
  {1118, dirnames},
  {1122, optflags},
  {1124, payloadformat},
  {1125, payloadcompressor},
  {1126, payloadflags},
  {1132, platform},
  {1140, filecolors},
  {1141, fileclass},
  {1142, classdict},
  {1143, filedependsx},
  {1144, filedependsn},
  {1145, filedependsdict},
  {5011, filedigestalgo}
  ].


