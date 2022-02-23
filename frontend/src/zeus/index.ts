/* eslint-disable */

import { AllTypesProps, ReturnTypes } from './const';
type ZEUS_INTERFACES = GraphQLTypes["Node"]
type ZEUS_UNIONS = never

export type ValueTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: AliasType<{
	/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query?:ValueTypes["Query"],
	/** The root query type must be a `Node` to work well with Relay 1 mutations. This just resolves to `query`. */
	nodeId?:boolean,
node?: [{	/** The globally unique `ID`. */
	nodeId:string},ValueTypes["Node"]],
akces?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Akce`. */
	orderBy?:ValueTypes["AkcesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AkceCondition"] | null},ValueTypes["AkcesConnection"]],
akceItems?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AkceItemCondition"] | null},ValueTypes["AkceItemsConnection"]],
aktualities?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AktualityCondition"] | null},ValueTypes["AktualitiesConnection"]],
dokumenties?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["DokumentyCondition"] | null},ValueTypes["DokumentiesConnection"]],
galerieDirs?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `GalerieDir`. */
	orderBy?:ValueTypes["GalerieDirsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["GalerieDirCondition"] | null},ValueTypes["GalerieDirsConnection"]],
galerieFotos?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["GalerieFotoCondition"] | null},ValueTypes["GalerieFotosConnection"]],
members?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Member`. */
	orderBy?:ValueTypes["MembersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["MemberCondition"] | null},ValueTypes["MembersConnection"]],
nabidkas?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaCondition"] | null},ValueTypes["NabidkasConnection"]],
nabidkaItems?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaItemCondition"] | null},ValueTypes["NabidkaItemsConnection"]],
pages?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Page`. */
	orderBy?:ValueTypes["PagesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PageCondition"] | null},ValueTypes["PagesConnection"]],
pageRevisions?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PageRevision`. */
	orderBy?:ValueTypes["PageRevisionsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PageRevisionCondition"] | null},ValueTypes["PageRevisionsConnection"]],
parameters?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Parameter`. */
	orderBy?:ValueTypes["ParametersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParameterCondition"] | null},ValueTypes["ParametersConnection"]],
paries?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyCondition"] | null},ValueTypes["PariesConnection"]],
paryNavrhs?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyNavrhCondition"] | null},ValueTypes["ParyNavrhsConnection"]],
permissions?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Permission`. */
	orderBy?:ValueTypes["PermissionsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PermissionCondition"] | null},ValueTypes["PermissionsConnection"]],
platbyCategories?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyCategory`. */
	orderBy?:ValueTypes["PlatbyCategoriesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyCategoryCondition"] | null},ValueTypes["PlatbyCategoriesConnection"]],
platbyCategoryGroups?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyCategoryGroupCondition"] | null},ValueTypes["PlatbyCategoryGroupsConnection"]],
platbyGroups?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyGroup`. */
	orderBy?:ValueTypes["PlatbyGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyGroupCondition"] | null},ValueTypes["PlatbyGroupsConnection"]],
platbyGroupSkupinas?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyGroupSkupinaCondition"] | null},ValueTypes["PlatbyGroupSkupinasConnection"]],
platbyItems?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyItemCondition"] | null},ValueTypes["PlatbyItemsConnection"]],
platbyRaws?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyRaw`. */
	orderBy?:ValueTypes["PlatbyRawsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyRawCondition"] | null},ValueTypes["PlatbyRawsConnection"]],
rozpis?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Rozpi`. */
	orderBy?:ValueTypes["RozpisOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RozpiCondition"] | null},ValueTypes["RozpisConnection"]],
rozpisItems?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RozpisItemCondition"] | null},ValueTypes["RozpisItemsConnection"]],
sessions?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Session`. */
	orderBy?:ValueTypes["SessionsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["SessionCondition"] | null},ValueTypes["SessionsConnection"]],
skupinies?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["SkupinyCondition"] | null},ValueTypes["SkupiniesConnection"]],
upozornenis?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniCondition"] | null},ValueTypes["UpozornenisConnection"]],
upozorneniSkupinies?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniSkupinyCondition"] | null},ValueTypes["UpozorneniSkupiniesConnection"]],
users?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UserCondition"] | null},ValueTypes["UsersConnection"]],
usersSkupinies?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `UsersSkupiny`. */
	orderBy?:ValueTypes["UsersSkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UsersSkupinyCondition"] | null},ValueTypes["UsersSkupiniesConnection"]],
videos?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Video`. */
	orderBy?:ValueTypes["VideosOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["VideoCondition"] | null},ValueTypes["VideosConnection"]],
videoLists?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `VideoList`. */
	orderBy?:ValueTypes["VideoListsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["VideoListCondition"] | null},ValueTypes["VideoListsConnection"]],
videoSources?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `VideoSource`. */
	orderBy?:ValueTypes["VideoSourcesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["VideoSourceCondition"] | null},ValueTypes["VideoSourcesConnection"]],
akce?: [{	aId:ValueTypes["BigInt"]},ValueTypes["Akce"]],
akceItem?: [{	aiId:ValueTypes["BigInt"]},ValueTypes["AkceItem"]],
aktuality?: [{	atId:ValueTypes["BigInt"]},ValueTypes["Aktuality"]],
dokumenty?: [{	dId:ValueTypes["BigInt"]},ValueTypes["Dokumenty"]],
galerieDir?: [{	gdId:ValueTypes["BigInt"]},ValueTypes["GalerieDir"]],
galerieFoto?: [{	gfId:ValueTypes["BigInt"]},ValueTypes["GalerieFoto"]],
nabidka?: [{	nId:ValueTypes["BigInt"]},ValueTypes["Nabidka"]],
nabidkaItem?: [{	niId:ValueTypes["BigInt"]},ValueTypes["NabidkaItem"]],
page?: [{	id:number},ValueTypes["Page"]],
pageByUrl?: [{	url:string},ValueTypes["Page"]],
pageRevision?: [{	revNumber:number,	id:number},ValueTypes["PageRevision"]],
parameter?: [{	paName:string},ValueTypes["Parameter"]],
pary?: [{	pId:ValueTypes["BigInt"]},ValueTypes["Pary"]],
paryNavrh?: [{	pnId:ValueTypes["BigInt"]},ValueTypes["ParyNavrh"]],
permission?: [{	peId:ValueTypes["BigInt"]},ValueTypes["Permission"]],
platbyCategory?: [{	pcId:ValueTypes["BigInt"]},ValueTypes["PlatbyCategory"]],
platbyCategoryGroup?: [{	pcgId:ValueTypes["BigInt"]},ValueTypes["PlatbyCategoryGroup"]],
platbyGroup?: [{	pgId:ValueTypes["BigInt"]},ValueTypes["PlatbyGroup"]],
platbyGroupSkupina?: [{	pgsId:ValueTypes["BigInt"]},ValueTypes["PlatbyGroupSkupina"]],
platbyItem?: [{	piId:ValueTypes["BigInt"]},ValueTypes["PlatbyItem"]],
platbyRaw?: [{	prId:ValueTypes["BigInt"]},ValueTypes["PlatbyRaw"]],
rozpi?: [{	rId:ValueTypes["BigInt"]},ValueTypes["Rozpi"]],
rozpisItem?: [{	riId:ValueTypes["BigInt"]},ValueTypes["RozpisItem"]],
session?: [{	ssId:string},ValueTypes["Session"]],
skupiny?: [{	sId:ValueTypes["BigInt"]},ValueTypes["Skupiny"]],
upozorneni?: [{	upId:ValueTypes["BigInt"]},ValueTypes["Upozorneni"]],
upozorneniSkupiny?: [{	upsId:ValueTypes["BigInt"]},ValueTypes["UpozorneniSkupiny"]],
user?: [{	uId:ValueTypes["BigInt"]},ValueTypes["User"]],
usersSkupiny?: [{	usId:ValueTypes["BigInt"]},ValueTypes["UsersSkupiny"]],
video?: [{	vId:ValueTypes["BigInt"]},ValueTypes["Video"]],
videoList?: [{	vlId:ValueTypes["BigInt"]},ValueTypes["VideoList"]],
videoSource?: [{	vsId:ValueTypes["BigInt"]},ValueTypes["VideoSource"]],
currentCoupleIds?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["CurrentCoupleIdsConnection"]],
	currentSessionId?:boolean,
	currentUserId?:boolean,
	getCurrentUser?:ValueTypes["User"],
reservationsForRange?: [{	startDate?:ValueTypes["Date"] | null,	endDate?:ValueTypes["Date"] | null,	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["NabidkasConnection"]],
schedulesForRange?: [{	startDate?:ValueTypes["Date"] | null,	endDate?:ValueTypes["Date"] | null,	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["RozpisConnection"]],
titleVideos?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["VideosConnection"]],
akceByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Akce`. */
	nodeId:string},ValueTypes["Akce"]],
akceItemByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `AkceItem`. */
	nodeId:string},ValueTypes["AkceItem"]],
aktualityByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Aktuality`. */
	nodeId:string},ValueTypes["Aktuality"]],
dokumentyByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Dokumenty`. */
	nodeId:string},ValueTypes["Dokumenty"]],
galerieDirByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `GalerieDir`. */
	nodeId:string},ValueTypes["GalerieDir"]],
galerieFotoByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `GalerieFoto`. */
	nodeId:string},ValueTypes["GalerieFoto"]],
nabidkaByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Nabidka`. */
	nodeId:string},ValueTypes["Nabidka"]],
nabidkaItemByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `NabidkaItem`. */
	nodeId:string},ValueTypes["NabidkaItem"]],
pageByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Page`. */
	nodeId:string},ValueTypes["Page"]],
pageRevisionByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PageRevision`. */
	nodeId:string},ValueTypes["PageRevision"]],
parameterByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Parameter`. */
	nodeId:string},ValueTypes["Parameter"]],
paryByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Pary`. */
	nodeId:string},ValueTypes["Pary"]],
paryNavrhByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `ParyNavrh`. */
	nodeId:string},ValueTypes["ParyNavrh"]],
permissionByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Permission`. */
	nodeId:string},ValueTypes["Permission"]],
platbyCategoryByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyCategory`. */
	nodeId:string},ValueTypes["PlatbyCategory"]],
platbyCategoryGroupByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyCategoryGroup`. */
	nodeId:string},ValueTypes["PlatbyCategoryGroup"]],
platbyGroupByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyGroup`. */
	nodeId:string},ValueTypes["PlatbyGroup"]],
platbyGroupSkupinaByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyGroupSkupina`. */
	nodeId:string},ValueTypes["PlatbyGroupSkupina"]],
platbyItemByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyItem`. */
	nodeId:string},ValueTypes["PlatbyItem"]],
platbyRawByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `PlatbyRaw`. */
	nodeId:string},ValueTypes["PlatbyRaw"]],
rozpiByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Rozpi`. */
	nodeId:string},ValueTypes["Rozpi"]],
rozpisItemByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `RozpisItem`. */
	nodeId:string},ValueTypes["RozpisItem"]],
sessionByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Session`. */
	nodeId:string},ValueTypes["Session"]],
skupinyByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Skupiny`. */
	nodeId:string},ValueTypes["Skupiny"]],
upozorneniByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Upozorneni`. */
	nodeId:string},ValueTypes["Upozorneni"]],
upozorneniSkupinyByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `UpozorneniSkupiny`. */
	nodeId:string},ValueTypes["UpozorneniSkupiny"]],
userByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `User`. */
	nodeId:string},ValueTypes["User"]],
usersSkupinyByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `UsersSkupiny`. */
	nodeId:string},ValueTypes["UsersSkupiny"]],
videoByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `Video`. */
	nodeId:string},ValueTypes["Video"]],
videoListByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `VideoList`. */
	nodeId:string},ValueTypes["VideoList"]],
videoSourceByNodeId?: [{	/** The globally unique `ID` to be used in selecting a single `VideoSource`. */
	nodeId:string},ValueTypes["VideoSource"]],
		__typename?: boolean
}>;
	/** An object with a globally unique `ID`. */
["Node"]:AliasType<{
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean;
		['...on Query']?: Omit<ValueTypes["Query"],keyof ValueTypes["Node"]>;
		['...on Akce']?: Omit<ValueTypes["Akce"],keyof ValueTypes["Node"]>;
		['...on AkceItem']?: Omit<ValueTypes["AkceItem"],keyof ValueTypes["Node"]>;
		['...on User']?: Omit<ValueTypes["User"],keyof ValueTypes["Node"]>;
		['...on Permission']?: Omit<ValueTypes["Permission"],keyof ValueTypes["Node"]>;
		['...on Skupiny']?: Omit<ValueTypes["Skupiny"],keyof ValueTypes["Node"]>;
		['...on PlatbyGroupSkupina']?: Omit<ValueTypes["PlatbyGroupSkupina"],keyof ValueTypes["Node"]>;
		['...on PlatbyGroup']?: Omit<ValueTypes["PlatbyGroup"],keyof ValueTypes["Node"]>;
		['...on PlatbyCategoryGroup']?: Omit<ValueTypes["PlatbyCategoryGroup"],keyof ValueTypes["Node"]>;
		['...on PlatbyCategory']?: Omit<ValueTypes["PlatbyCategory"],keyof ValueTypes["Node"]>;
		['...on PlatbyItem']?: Omit<ValueTypes["PlatbyItem"],keyof ValueTypes["Node"]>;
		['...on PlatbyRaw']?: Omit<ValueTypes["PlatbyRaw"],keyof ValueTypes["Node"]>;
		['...on UpozorneniSkupiny']?: Omit<ValueTypes["UpozorneniSkupiny"],keyof ValueTypes["Node"]>;
		['...on Upozorneni']?: Omit<ValueTypes["Upozorneni"],keyof ValueTypes["Node"]>;
		['...on Aktuality']?: Omit<ValueTypes["Aktuality"],keyof ValueTypes["Node"]>;
		['...on GalerieFoto']?: Omit<ValueTypes["GalerieFoto"],keyof ValueTypes["Node"]>;
		['...on GalerieDir']?: Omit<ValueTypes["GalerieDir"],keyof ValueTypes["Node"]>;
		['...on Nabidka']?: Omit<ValueTypes["Nabidka"],keyof ValueTypes["Node"]>;
		['...on NabidkaItem']?: Omit<ValueTypes["NabidkaItem"],keyof ValueTypes["Node"]>;
		['...on Pary']?: Omit<ValueTypes["Pary"],keyof ValueTypes["Node"]>;
		['...on RozpisItem']?: Omit<ValueTypes["RozpisItem"],keyof ValueTypes["Node"]>;
		['...on Rozpi']?: Omit<ValueTypes["Rozpi"],keyof ValueTypes["Node"]>;
		['...on Session']?: Omit<ValueTypes["Session"],keyof ValueTypes["Node"]>;
		['...on Dokumenty']?: Omit<ValueTypes["Dokumenty"],keyof ValueTypes["Node"]>;
		['...on ParyNavrh']?: Omit<ValueTypes["ParyNavrh"],keyof ValueTypes["Node"]>;
		['...on Page']?: Omit<ValueTypes["Page"],keyof ValueTypes["Node"]>;
		['...on PageRevision']?: Omit<ValueTypes["PageRevision"],keyof ValueTypes["Node"]>;
		['...on Parameter']?: Omit<ValueTypes["Parameter"],keyof ValueTypes["Node"]>;
		['...on UsersSkupiny']?: Omit<ValueTypes["UsersSkupiny"],keyof ValueTypes["Node"]>;
		['...on Video']?: Omit<ValueTypes["Video"],keyof ValueTypes["Node"]>;
		['...on VideoList']?: Omit<ValueTypes["VideoList"],keyof ValueTypes["Node"]>;
		['...on VideoSource']?: Omit<ValueTypes["VideoSource"],keyof ValueTypes["Node"]>;
		__typename?: boolean
}>;
	/** A connection to a list of `Akce` values. */
["AkcesConnection"]: AliasType<{
	/** A list of `Akce` objects. */
	nodes?:ValueTypes["Akce"],
	/** A list of edges which contains the `Akce` and cursor to aid in pagination. */
	edges?:ValueTypes["AkcesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Akce` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Akce"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	aId?:boolean,
	aJmeno?:boolean,
	aKde?:boolean,
	aInfo?:boolean,
	aOd?:boolean,
	aDo?:boolean,
	aKapacita?:boolean,
	aDokumenty?:boolean,
	aTimestamp?:boolean,
	aLock?:boolean,
	aVisible?:boolean,
akceItemsByAiIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AkceItemCondition"] | null},ValueTypes["AkceItemsConnection"]],
		__typename?: boolean
}>;
	/** A signed eight-byte integer. The upper big integer values are greater than the
max value for a JavaScript number. Therefore all big integers will be output as
strings and not numbers. */
["BigInt"]:unknown;
	/** The day, does not include a time. */
["Date"]:unknown;
	/** A point in time as described by the [ISO
8601](https://en.wikipedia.org/wiki/ISO_8601) standard. May or may not include a timezone. */
["Datetime"]:unknown;
	/** A connection to a list of `AkceItem` values. */
["AkceItemsConnection"]: AliasType<{
	/** A list of `AkceItem` objects. */
	nodes?:ValueTypes["AkceItem"],
	/** A list of edges which contains the `AkceItem` and cursor to aid in pagination. */
	edges?:ValueTypes["AkceItemsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `AkceItem` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["AkceItem"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	aiId?:boolean,
	aiIdRodic?:boolean,
	aiUser?:boolean,
	aiRokNarozeni?:boolean,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ValueTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ValueTypes["User"],
		__typename?: boolean
}>;
	["User"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	uId?:boolean,
	uLogin?:boolean,
	uPass?:boolean,
	uJmeno?:boolean,
	uPrijmeni?:boolean,
	uPohlavi?:boolean,
	uEmail?:boolean,
	uTelefon?:boolean,
	uNarozeni?:boolean,
	uRodneCislo?:boolean,
	uPoznamky?:boolean,
	uTimestamp?:boolean,
	uLevel?:boolean,
	uGroup?:boolean,
	uSkupina?:boolean,
	uDancer?:boolean,
	uBan?:boolean,
	uLock?:boolean,
	uConfirmed?:boolean,
	uSystem?:boolean,
	uStreet?:boolean,
	uConscriptionNumber?:boolean,
	uOrientationNumber?:boolean,
	uDistrict?:boolean,
	uCity?:boolean,
	uPostalCode?:boolean,
	uNationality?:boolean,
	uMemberSince?:boolean,
	uMemberUntil?:boolean,
	uCreatedAt?:boolean,
	uTeacher?:boolean,
	uGdprSignedAt?:boolean,
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ValueTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ValueTypes["Skupiny"],
aktualitiesByAtKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AktualityCondition"] | null},ValueTypes["AktualitiesConnection"]],
nabidkasByNTrener?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaCondition"] | null},ValueTypes["NabidkasConnection"]],
rozpisByRTrener?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Rozpi`. */
	orderBy?:ValueTypes["RozpisOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RozpiCondition"] | null},ValueTypes["RozpisConnection"]],
sessionsBySsUser?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Session`. */
	orderBy?:ValueTypes["SessionsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["SessionCondition"] | null},ValueTypes["SessionsConnection"]],
akceItemsByAiUser?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AkceItemCondition"] | null},ValueTypes["AkceItemsConnection"]],
dokumentiesByDKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["DokumentyCondition"] | null},ValueTypes["DokumentiesConnection"]],
galerieFotosByGfKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["GalerieFotoCondition"] | null},ValueTypes["GalerieFotosConnection"]],
platbyItemsByPiIdUser?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyItemCondition"] | null},ValueTypes["PlatbyItemsConnection"]],
pariesByPIdPartner?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyCondition"] | null},ValueTypes["PariesConnection"]],
paryNavrhsByPnNavrhl?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyNavrhCondition"] | null},ValueTypes["ParyNavrhsConnection"]],
paryNavrhsByPnPartner?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyNavrhCondition"] | null},ValueTypes["ParyNavrhsConnection"]],
paryNavrhsByPnPartnerka?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyNavrhCondition"] | null},ValueTypes["ParyNavrhsConnection"]],
upozornenisByUpKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniCondition"] | null},ValueTypes["UpozornenisConnection"]],
		__typename?: boolean
}>;
	["Permission"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	peId?:boolean,
	peName?:boolean,
	peDescription?:boolean,
	peAkce?:boolean,
	peAktuality?:boolean,
	peAnkety?:boolean,
	peDokumenty?:boolean,
	peGalerie?:boolean,
	peInzerce?:boolean,
	peKonzole?:boolean,
	peNabidka?:boolean,
	peNastenka?:boolean,
	peNovinky?:boolean,
	pePary?:boolean,
	pePlatby?:boolean,
	pePermissions?:boolean,
	peRozpis?:boolean,
	peSkupiny?:boolean,
	peUsers?:boolean,
	peMain?:boolean,
usersByUGroup?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UserCondition"] | null},ValueTypes["UsersConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `User` values. */
["UsersConnection"]: AliasType<{
	/** A list of `User` objects. */
	nodes?:ValueTypes["User"],
	/** A list of edges which contains the `User` and cursor to aid in pagination. */
	edges?:ValueTypes["UsersEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `User` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `User` edge in the connection. */
["UsersEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `User` at the end of the edge. */
	node?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** A location in a connection that can be used for resuming pagination. */
["Cursor"]:unknown;
	/** Information about pagination in a connection. */
["PageInfo"]: AliasType<{
	/** When paginating forwards, are there more items? */
	hasNextPage?:boolean,
	/** When paginating backwards, are there more items? */
	hasPreviousPage?:boolean,
	/** When paginating backwards, the cursor to continue. */
	startCursor?:boolean,
	/** When paginating forwards, the cursor to continue. */
	endCursor?:boolean,
		__typename?: boolean
}>;
	/** Methods to use when ordering `User`. */
["UsersOrderBy"]:UsersOrderBy;
	/** A condition to be used against `User` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["UserCondition"]: {
	/** Checks for equality with the object’s `uId` field. */
	uId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uLogin` field. */
	uLogin?:string | null,
	/** Checks for equality with the object’s `uPass` field. */
	uPass?:string | null,
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?:string | null,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?:string | null,
	/** Checks for equality with the object’s `uPohlavi` field. */
	uPohlavi?:string | null,
	/** Checks for equality with the object’s `uEmail` field. */
	uEmail?:string | null,
	/** Checks for equality with the object’s `uTelefon` field. */
	uTelefon?:string | null,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `uRodneCislo` field. */
	uRodneCislo?:string | null,
	/** Checks for equality with the object’s `uPoznamky` field. */
	uPoznamky?:string | null,
	/** Checks for equality with the object’s `uTimestamp` field. */
	uTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uLevel` field. */
	uLevel?:number | null,
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uDancer` field. */
	uDancer?:boolean | null,
	/** Checks for equality with the object’s `uBan` field. */
	uBan?:boolean | null,
	/** Checks for equality with the object’s `uLock` field. */
	uLock?:boolean | null,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?:boolean | null,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?:boolean | null,
	/** Checks for equality with the object’s `uStreet` field. */
	uStreet?:string | null,
	/** Checks for equality with the object’s `uConscriptionNumber` field. */
	uConscriptionNumber?:string | null,
	/** Checks for equality with the object’s `uOrientationNumber` field. */
	uOrientationNumber?:string | null,
	/** Checks for equality with the object’s `uDistrict` field. */
	uDistrict?:string | null,
	/** Checks for equality with the object’s `uCity` field. */
	uCity?:string | null,
	/** Checks for equality with the object’s `uPostalCode` field. */
	uPostalCode?:string | null,
	/** Checks for equality with the object’s `uNationality` field. */
	uNationality?:string | null,
	/** Checks for equality with the object’s `uMemberSince` field. */
	uMemberSince?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uMemberUntil` field. */
	uMemberUntil?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uCreatedAt` field. */
	uCreatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uTeacher` field. */
	uTeacher?:boolean | null,
	/** Checks for equality with the object’s `uGdprSignedAt` field. */
	uGdprSignedAt?:ValueTypes["Datetime"] | null
};
	["Skupiny"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	sId?:boolean,
	sName?:boolean,
	sDescription?:boolean,
	sColorRgb?:boolean,
	sColorText?:boolean,
	sLocation?:boolean,
	sVisible?:boolean,
usersByUSkupina?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UserCondition"] | null},ValueTypes["UsersConnection"]],
platbyGroupSkupinasByPgsIdSkupina?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyGroupSkupinaCondition"] | null},ValueTypes["PlatbyGroupSkupinasConnection"]],
upozorneniSkupiniesByUpsIdSkupina?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniSkupinyCondition"] | null},ValueTypes["UpozorneniSkupiniesConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `PlatbyGroupSkupina` values. */
["PlatbyGroupSkupinasConnection"]: AliasType<{
	/** A list of `PlatbyGroupSkupina` objects. */
	nodes?:ValueTypes["PlatbyGroupSkupina"],
	/** A list of edges which contains the `PlatbyGroupSkupina` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyGroupSkupinasEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyGroupSkupina` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["PlatbyGroupSkupina"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pgsId?:boolean,
	pgsIdSkupina?:boolean,
	pgsIdGroup?:boolean,
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ValueTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ValueTypes["PlatbyGroup"],
		__typename?: boolean
}>;
	["PlatbyGroup"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pgId?:boolean,
	pgType?:boolean,
	pgName?:boolean,
	pgDescription?:boolean,
	pgBase?:boolean,
platbyCategoryGroupsByPcgIdGroup?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyCategoryGroupCondition"] | null},ValueTypes["PlatbyCategoryGroupsConnection"]],
platbyGroupSkupinasByPgsIdGroup?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyGroupSkupinaCondition"] | null},ValueTypes["PlatbyGroupSkupinasConnection"]],
		__typename?: boolean
}>;
	/** A floating point number that requires more precision than IEEE 754 binary 64 */
["BigFloat"]:unknown;
	/** A connection to a list of `PlatbyCategoryGroup` values. */
["PlatbyCategoryGroupsConnection"]: AliasType<{
	/** A list of `PlatbyCategoryGroup` objects. */
	nodes?:ValueTypes["PlatbyCategoryGroup"],
	/** A list of edges which contains the `PlatbyCategoryGroup` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyCategoryGroupsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyCategoryGroup` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["PlatbyCategoryGroup"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pcgId?:boolean,
	pcgIdGroup?:boolean,
	pcgIdCategory?:boolean,
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ValueTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ValueTypes["PlatbyCategory"],
		__typename?: boolean
}>;
	["PlatbyCategory"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pcId?:boolean,
	pcName?:boolean,
	pcSymbol?:boolean,
	pcAmount?:boolean,
	pcDateDue?:boolean,
	pcValidFrom?:boolean,
	pcValidTo?:boolean,
	pcUseBase?:boolean,
	pcUsePrefix?:boolean,
	pcArchive?:boolean,
	pcVisible?:boolean,
platbyCategoryGroupsByPcgIdCategory?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyCategoryGroupCondition"] | null},ValueTypes["PlatbyCategoryGroupsConnection"]],
platbyItemsByPiIdCategory?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyItemCondition"] | null},ValueTypes["PlatbyItemsConnection"]],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyCategoryGroup`. */
["PlatbyCategoryGroupsOrderBy"]:PlatbyCategoryGroupsOrderBy;
	/** A condition to be used against `PlatbyCategoryGroup` object types. All fields
are tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryGroupCondition"]: {
	/** Checks for equality with the object’s `pcgId` field. */
	pcgId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pcgIdGroup` field. */
	pcgIdGroup?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pcgIdCategory` field. */
	pcgIdCategory?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `PlatbyItem` values. */
["PlatbyItemsConnection"]: AliasType<{
	/** A list of `PlatbyItem` objects. */
	nodes?:ValueTypes["PlatbyItem"],
	/** A list of edges which contains the `PlatbyItem` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyItemsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyItem` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["PlatbyItem"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	piId?:boolean,
	piIdUser?:boolean,
	piIdCategory?:boolean,
	piIdRaw?:boolean,
	piAmount?:boolean,
	piDate?:boolean,
	piPrefix?:boolean,
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ValueTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ValueTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ValueTypes["PlatbyRaw"],
		__typename?: boolean
}>;
	["PlatbyRaw"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	prId?:boolean,
	prRaw?:boolean,
	prHash?:boolean,
	prSorted?:boolean,
	prDiscarded?:boolean,
platbyItemsByPiIdRaw?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PlatbyItemCondition"] | null},ValueTypes["PlatbyItemsConnection"]],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyItem`. */
["PlatbyItemsOrderBy"]:PlatbyItemsOrderBy;
	/** A condition to be used against `PlatbyItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyItemCondition"]: {
	/** Checks for equality with the object’s `piId` field. */
	piId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `piIdUser` field. */
	piIdUser?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `piIdCategory` field. */
	piIdCategory?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `piIdRaw` field. */
	piIdRaw?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `piAmount` field. */
	piAmount?:ValueTypes["BigFloat"] | null,
	/** Checks for equality with the object’s `piDate` field. */
	piDate?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `piPrefix` field. */
	piPrefix?:number | null
};
	/** A `PlatbyItem` edge in the connection. */
["PlatbyItemsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyItem` at the end of the edge. */
	node?:ValueTypes["PlatbyItem"],
		__typename?: boolean
}>;
	/** A `PlatbyCategoryGroup` edge in the connection. */
["PlatbyCategoryGroupsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyCategoryGroup` at the end of the edge. */
	node?:ValueTypes["PlatbyCategoryGroup"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyGroupSkupina`. */
["PlatbyGroupSkupinasOrderBy"]:PlatbyGroupSkupinasOrderBy;
	/** A condition to be used against `PlatbyGroupSkupina` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyGroupSkupinaCondition"]: {
	/** Checks for equality with the object’s `pgsId` field. */
	pgsId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pgsIdSkupina` field. */
	pgsIdSkupina?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pgsIdGroup` field. */
	pgsIdGroup?:ValueTypes["BigInt"] | null
};
	/** A `PlatbyGroupSkupina` edge in the connection. */
["PlatbyGroupSkupinasEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyGroupSkupina` at the end of the edge. */
	node?:ValueTypes["PlatbyGroupSkupina"],
		__typename?: boolean
}>;
	/** A connection to a list of `UpozorneniSkupiny` values. */
["UpozorneniSkupiniesConnection"]: AliasType<{
	/** A list of `UpozorneniSkupiny` objects. */
	nodes?:ValueTypes["UpozorneniSkupiny"],
	/** A list of edges which contains the `UpozorneniSkupiny` and cursor to aid in pagination. */
	edges?:ValueTypes["UpozorneniSkupiniesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `UpozorneniSkupiny` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["UpozorneniSkupiny"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	upsId?:boolean,
	upsIdRodic?:boolean,
	upsIdSkupina?:boolean,
	upsColor?:boolean,
	upsPopis?:boolean,
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ValueTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ValueTypes["Skupiny"],
		__typename?: boolean
}>;
	["Upozorneni"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	upId?:boolean,
	upKdo?:boolean,
	upNadpis?:boolean,
	upText?:boolean,
	upBarvy?:boolean,
	upLock?:boolean,
	upTimestamp?:boolean,
	upTimestampAdd?:boolean,
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ValueTypes["User"],
upozorneniSkupiniesByUpsIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniSkupinyCondition"] | null},ValueTypes["UpozorneniSkupiniesConnection"]],
		__typename?: boolean
}>;
	/** Methods to use when ordering `UpozorneniSkupiny`. */
["UpozorneniSkupiniesOrderBy"]:UpozorneniSkupiniesOrderBy;
	/** A condition to be used against `UpozorneniSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UpozorneniSkupinyCondition"]: {
	/** Checks for equality with the object’s `upsId` field. */
	upsId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upsIdRodic` field. */
	upsIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upsIdSkupina` field. */
	upsIdSkupina?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upsColor` field. */
	upsColor?:string | null,
	/** Checks for equality with the object’s `upsPopis` field. */
	upsPopis?:string | null
};
	/** A `UpozorneniSkupiny` edge in the connection. */
["UpozorneniSkupiniesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `UpozorneniSkupiny` at the end of the edge. */
	node?:ValueTypes["UpozorneniSkupiny"],
		__typename?: boolean
}>;
	/** A connection to a list of `Aktuality` values. */
["AktualitiesConnection"]: AliasType<{
	/** A list of `Aktuality` objects. */
	nodes?:ValueTypes["Aktuality"],
	/** A list of edges which contains the `Aktuality` and cursor to aid in pagination. */
	edges?:ValueTypes["AktualitiesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Aktuality` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Aktuality"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	atId?:boolean,
	atKdo?:boolean,
	atKat?:boolean,
	atJmeno?:boolean,
	atText?:boolean,
	atPreview?:boolean,
	atFoto?:boolean,
	atFotoMain?:boolean,
	atTimestamp?:boolean,
	atTimestampAdd?:boolean,
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ValueTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ValueTypes["GalerieFoto"],
		__typename?: boolean
}>;
	["GalerieFoto"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	gfId?:boolean,
	gfIdRodic?:boolean,
	gfName?:boolean,
	gfPath?:boolean,
	gfKdo?:boolean,
	gfTimestamp?:boolean,
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ValueTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ValueTypes["User"],
aktualitiesByAtFotoMain?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AktualityCondition"] | null},ValueTypes["AktualitiesConnection"]],
		__typename?: boolean
}>;
	["GalerieDir"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	gdId?:boolean,
	gdIdRodic?:boolean,
	gdName?:boolean,
	gdLevel?:boolean,
	gdPath?:boolean,
	gdHidden?:boolean,
galerieFotosByGfIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["GalerieFotoCondition"] | null},ValueTypes["GalerieFotosConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `GalerieFoto` values. */
["GalerieFotosConnection"]: AliasType<{
	/** A list of `GalerieFoto` objects. */
	nodes?:ValueTypes["GalerieFoto"],
	/** A list of edges which contains the `GalerieFoto` and cursor to aid in pagination. */
	edges?:ValueTypes["GalerieFotosEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `GalerieFoto` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `GalerieFoto` edge in the connection. */
["GalerieFotosEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `GalerieFoto` at the end of the edge. */
	node?:ValueTypes["GalerieFoto"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `GalerieFoto`. */
["GalerieFotosOrderBy"]:GalerieFotosOrderBy;
	/** A condition to be used against `GalerieFoto` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieFotoCondition"]: {
	/** Checks for equality with the object’s `gfId` field. */
	gfId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `gfIdRodic` field. */
	gfIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `gfName` field. */
	gfName?:string | null,
	/** Checks for equality with the object’s `gfPath` field. */
	gfPath?:string | null,
	/** Checks for equality with the object’s `gfKdo` field. */
	gfKdo?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `gfTimestamp` field. */
	gfTimestamp?:ValueTypes["Datetime"] | null
};
	/** Methods to use when ordering `Aktuality`. */
["AktualitiesOrderBy"]:AktualitiesOrderBy;
	/** A condition to be used against `Aktuality` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AktualityCondition"]: {
	/** Checks for equality with the object’s `atId` field. */
	atId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `atKdo` field. */
	atKdo?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `atKat` field. */
	atKat?:string | null,
	/** Checks for equality with the object’s `atJmeno` field. */
	atJmeno?:string | null,
	/** Checks for equality with the object’s `atText` field. */
	atText?:string | null,
	/** Checks for equality with the object’s `atPreview` field. */
	atPreview?:string | null,
	/** Checks for equality with the object’s `atFoto` field. */
	atFoto?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `atFotoMain` field. */
	atFotoMain?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `atTimestamp` field. */
	atTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `atTimestampAdd` field. */
	atTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** A `Aktuality` edge in the connection. */
["AktualitiesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Aktuality` at the end of the edge. */
	node?:ValueTypes["Aktuality"],
		__typename?: boolean
}>;
	/** A connection to a list of `Nabidka` values. */
["NabidkasConnection"]: AliasType<{
	/** A list of `Nabidka` objects. */
	nodes?:ValueTypes["Nabidka"],
	/** A list of edges which contains the `Nabidka` and cursor to aid in pagination. */
	edges?:ValueTypes["NabidkasEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Nabidka` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Nabidka"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	nId?:boolean,
	nTrener?:boolean,
	nPocetHod?:boolean,
	nMaxPocetHod?:boolean,
	nOd?:boolean,
	nDo?:boolean,
	nVisible?:boolean,
	nLock?:boolean,
	nTimestamp?:boolean,
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ValueTypes["User"],
nabidkaItemsByNiIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaItemCondition"] | null},ValueTypes["NabidkaItemsConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `NabidkaItem` values. */
["NabidkaItemsConnection"]: AliasType<{
	/** A list of `NabidkaItem` objects. */
	nodes?:ValueTypes["NabidkaItem"],
	/** A list of edges which contains the `NabidkaItem` and cursor to aid in pagination. */
	edges?:ValueTypes["NabidkaItemsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `NabidkaItem` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["NabidkaItem"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	niId?:boolean,
	niIdRodic?:boolean,
	niPartner?:boolean,
	niPocetHod?:boolean,
	niLock?:boolean,
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ValueTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ValueTypes["Pary"],
		__typename?: boolean
}>;
	["Pary"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pId?:boolean,
	pIdPartner?:boolean,
	pIdPartnerka?:boolean,
	pSttTrida?:boolean,
	pSttBody?:boolean,
	pSttFinale?:boolean,
	pLatTrida?:boolean,
	pLatBody?:boolean,
	pLatFinale?:boolean,
	pHodnoceni?:boolean,
	pArchiv?:boolean,
	pTimestampAdd?:boolean,
	pTimestampArchive?:boolean,
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ValueTypes["User"],
nabidkaItemsByNiPartner?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaItemCondition"] | null},ValueTypes["NabidkaItemsConnection"]],
rozpisItemsByRiPartner?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RozpisItemCondition"] | null},ValueTypes["RozpisItemsConnection"]],
		__typename?: boolean
}>;
	["ParyPSttTrida"]:ParyPSttTrida;
	["ParyPLatTrida"]:ParyPLatTrida;
	/** Methods to use when ordering `NabidkaItem`. */
["NabidkaItemsOrderBy"]:NabidkaItemsOrderBy;
	/** A condition to be used against `NabidkaItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["NabidkaItemCondition"]: {
	/** Checks for equality with the object’s `niId` field. */
	niId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `niIdRodic` field. */
	niIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `niPartner` field. */
	niPartner?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `niPocetHod` field. */
	niPocetHod?:number | null,
	/** Checks for equality with the object’s `niLock` field. */
	niLock?:boolean | null
};
	/** A connection to a list of `RozpisItem` values. */
["RozpisItemsConnection"]: AliasType<{
	/** A list of `RozpisItem` objects. */
	nodes?:ValueTypes["RozpisItem"],
	/** A list of edges which contains the `RozpisItem` and cursor to aid in pagination. */
	edges?:ValueTypes["RozpisItemsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `RozpisItem` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["RozpisItem"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	riId?:boolean,
	riIdRodic?:boolean,
	riPartner?:boolean,
	riOd?:boolean,
	riDo?:boolean,
	riLock?:boolean,
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ValueTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ValueTypes["Pary"],
		__typename?: boolean
}>;
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:unknown;
	["Rozpi"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	rId?:boolean,
	rTrener?:boolean,
	rKde?:boolean,
	rDatum?:boolean,
	rVisible?:boolean,
	rLock?:boolean,
	rTimestamp?:boolean,
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ValueTypes["User"],
rozpisItemsByRiIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RozpisItemCondition"] | null},ValueTypes["RozpisItemsConnection"]],
		__typename?: boolean
}>;
	/** Methods to use when ordering `RozpisItem`. */
["RozpisItemsOrderBy"]:RozpisItemsOrderBy;
	/** A condition to be used against `RozpisItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["RozpisItemCondition"]: {
	/** Checks for equality with the object’s `riId` field. */
	riId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `riIdRodic` field. */
	riIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `riPartner` field. */
	riPartner?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `riOd` field. */
	riOd?:ValueTypes["Time"] | null,
	/** Checks for equality with the object’s `riDo` field. */
	riDo?:ValueTypes["Time"] | null,
	/** Checks for equality with the object’s `riLock` field. */
	riLock?:boolean | null
};
	/** A `RozpisItem` edge in the connection. */
["RozpisItemsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `RozpisItem` at the end of the edge. */
	node?:ValueTypes["RozpisItem"],
		__typename?: boolean
}>;
	/** A `NabidkaItem` edge in the connection. */
["NabidkaItemsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `NabidkaItem` at the end of the edge. */
	node?:ValueTypes["NabidkaItem"],
		__typename?: boolean
}>;
	/** A `Nabidka` edge in the connection. */
["NabidkasEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Nabidka` at the end of the edge. */
	node?:ValueTypes["Nabidka"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Nabidka`. */
["NabidkasOrderBy"]:NabidkasOrderBy;
	/** A condition to be used against `Nabidka` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["NabidkaCondition"]: {
	/** Checks for equality with the object’s `nId` field. */
	nId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `nTrener` field. */
	nTrener?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `nPocetHod` field. */
	nPocetHod?:number | null,
	/** Checks for equality with the object’s `nMaxPocetHod` field. */
	nMaxPocetHod?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `nOd` field. */
	nOd?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `nDo` field. */
	nDo?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `nVisible` field. */
	nVisible?:boolean | null,
	/** Checks for equality with the object’s `nLock` field. */
	nLock?:boolean | null,
	/** Checks for equality with the object’s `nTimestamp` field. */
	nTimestamp?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `Rozpi` values. */
["RozpisConnection"]: AliasType<{
	/** A list of `Rozpi` objects. */
	nodes?:ValueTypes["Rozpi"],
	/** A list of edges which contains the `Rozpi` and cursor to aid in pagination. */
	edges?:ValueTypes["RozpisEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Rozpi` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Rozpi` edge in the connection. */
["RozpisEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Rozpi` at the end of the edge. */
	node?:ValueTypes["Rozpi"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Rozpi`. */
["RozpisOrderBy"]:RozpisOrderBy;
	/** A condition to be used against `Rozpi` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RozpiCondition"]: {
	/** Checks for equality with the object’s `rId` field. */
	rId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `rTrener` field. */
	rTrener?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `rKde` field. */
	rKde?:string | null,
	/** Checks for equality with the object’s `rDatum` field. */
	rDatum?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `rVisible` field. */
	rVisible?:boolean | null,
	/** Checks for equality with the object’s `rLock` field. */
	rLock?:boolean | null,
	/** Checks for equality with the object’s `rTimestamp` field. */
	rTimestamp?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `Session` values. */
["SessionsConnection"]: AliasType<{
	/** A list of `Session` objects. */
	nodes?:ValueTypes["Session"],
	/** A list of edges which contains the `Session` and cursor to aid in pagination. */
	edges?:ValueTypes["SessionsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Session` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Session"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	ssId?:boolean,
	ssData?:boolean,
	ssUpdatedAt?:boolean,
	ssLifetime?:boolean,
	ssUser?:boolean,
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** A `Session` edge in the connection. */
["SessionsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Session` at the end of the edge. */
	node?:ValueTypes["Session"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Session`. */
["SessionsOrderBy"]:SessionsOrderBy;
	/** A condition to be used against `Session` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SessionCondition"]: {
	/** Checks for equality with the object’s `ssId` field. */
	ssId?:string | null,
	/** Checks for equality with the object’s `ssData` field. */
	ssData?:string | null,
	/** Checks for equality with the object’s `ssUpdatedAt` field. */
	ssUpdatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `ssLifetime` field. */
	ssLifetime?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `ssUser` field. */
	ssUser?:ValueTypes["BigInt"] | null
};
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]:AkceItemsOrderBy;
	/** A condition to be used against `AkceItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AkceItemCondition"]: {
	/** Checks for equality with the object’s `aiId` field. */
	aiId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `aiIdRodic` field. */
	aiIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `aiUser` field. */
	aiUser?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `aiRokNarozeni` field. */
	aiRokNarozeni?:number | null
};
	/** A connection to a list of `Dokumenty` values. */
["DokumentiesConnection"]: AliasType<{
	/** A list of `Dokumenty` objects. */
	nodes?:ValueTypes["Dokumenty"],
	/** A list of edges which contains the `Dokumenty` and cursor to aid in pagination. */
	edges?:ValueTypes["DokumentiesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Dokumenty` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Dokumenty"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	dId?:boolean,
	dPath?:boolean,
	dName?:boolean,
	dFilename?:boolean,
	dKategorie?:boolean,
	dKdo?:boolean,
	dTimestamp?:boolean,
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** A `Dokumenty` edge in the connection. */
["DokumentiesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Dokumenty` at the end of the edge. */
	node?:ValueTypes["Dokumenty"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Dokumenty`. */
["DokumentiesOrderBy"]:DokumentiesOrderBy;
	/** A condition to be used against `Dokumenty` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["DokumentyCondition"]: {
	/** Checks for equality with the object’s `dId` field. */
	dId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `dPath` field. */
	dPath?:string | null,
	/** Checks for equality with the object’s `dName` field. */
	dName?:string | null,
	/** Checks for equality with the object’s `dFilename` field. */
	dFilename?:string | null,
	/** Checks for equality with the object’s `dKategorie` field. */
	dKategorie?:number | null,
	/** Checks for equality with the object’s `dKdo` field. */
	dKdo?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `dTimestamp` field. */
	dTimestamp?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `Pary` values. */
["PariesConnection"]: AliasType<{
	/** A list of `Pary` objects. */
	nodes?:ValueTypes["Pary"],
	/** A list of edges which contains the `Pary` and cursor to aid in pagination. */
	edges?:ValueTypes["PariesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Pary` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Pary` edge in the connection. */
["PariesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Pary` at the end of the edge. */
	node?:ValueTypes["Pary"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Pary`. */
["PariesOrderBy"]:PariesOrderBy;
	/** A condition to be used against `Pary` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["ParyCondition"]: {
	/** Checks for equality with the object’s `pId` field. */
	pId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pIdPartner` field. */
	pIdPartner?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pIdPartnerka` field. */
	pIdPartnerka?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pSttTrida` field. */
	pSttTrida?:ValueTypes["ParyPSttTrida"] | null,
	/** Checks for equality with the object’s `pSttBody` field. */
	pSttBody?:number | null,
	/** Checks for equality with the object’s `pSttFinale` field. */
	pSttFinale?:boolean | null,
	/** Checks for equality with the object’s `pLatTrida` field. */
	pLatTrida?:ValueTypes["ParyPLatTrida"] | null,
	/** Checks for equality with the object’s `pLatBody` field. */
	pLatBody?:number | null,
	/** Checks for equality with the object’s `pLatFinale` field. */
	pLatFinale?:boolean | null,
	/** Checks for equality with the object’s `pHodnoceni` field. */
	pHodnoceni?:number | null,
	/** Checks for equality with the object’s `pArchiv` field. */
	pArchiv?:boolean | null,
	/** Checks for equality with the object’s `pTimestampAdd` field. */
	pTimestampAdd?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `pTimestampArchive` field. */
	pTimestampArchive?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `ParyNavrh` values. */
["ParyNavrhsConnection"]: AliasType<{
	/** A list of `ParyNavrh` objects. */
	nodes?:ValueTypes["ParyNavrh"],
	/** A list of edges which contains the `ParyNavrh` and cursor to aid in pagination. */
	edges?:ValueTypes["ParyNavrhsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `ParyNavrh` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["ParyNavrh"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	pnId?:boolean,
	pnNavrhl?:boolean,
	pnPartner?:boolean,
	pnPartnerka?:boolean,
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** A `ParyNavrh` edge in the connection. */
["ParyNavrhsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `ParyNavrh` at the end of the edge. */
	node?:ValueTypes["ParyNavrh"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `ParyNavrh`. */
["ParyNavrhsOrderBy"]:ParyNavrhsOrderBy;
	/** A condition to be used against `ParyNavrh` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParyNavrhCondition"]: {
	/** Checks for equality with the object’s `pnId` field. */
	pnId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pnNavrhl` field. */
	pnNavrhl?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pnPartner` field. */
	pnPartner?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pnPartnerka` field. */
	pnPartnerka?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `Upozorneni` values. */
["UpozornenisConnection"]: AliasType<{
	/** A list of `Upozorneni` objects. */
	nodes?:ValueTypes["Upozorneni"],
	/** A list of edges which contains the `Upozorneni` and cursor to aid in pagination. */
	edges?:ValueTypes["UpozornenisEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Upozorneni` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Upozorneni` edge in the connection. */
["UpozornenisEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Upozorneni` at the end of the edge. */
	node?:ValueTypes["Upozorneni"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Upozorneni`. */
["UpozornenisOrderBy"]:UpozornenisOrderBy;
	/** A condition to be used against `Upozorneni` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["UpozorneniCondition"]: {
	/** Checks for equality with the object’s `upId` field. */
	upId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upKdo` field. */
	upKdo?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upNadpis` field. */
	upNadpis?:string | null,
	/** Checks for equality with the object’s `upText` field. */
	upText?:string | null,
	/** Checks for equality with the object’s `upBarvy` field. */
	upBarvy?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `upLock` field. */
	upLock?:boolean | null,
	/** Checks for equality with the object’s `upTimestamp` field. */
	upTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `upTimestampAdd` field. */
	upTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** A `AkceItem` edge in the connection. */
["AkceItemsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `AkceItem` at the end of the edge. */
	node?:ValueTypes["AkceItem"],
		__typename?: boolean
}>;
	/** A `Akce` edge in the connection. */
["AkcesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Akce` at the end of the edge. */
	node?:ValueTypes["Akce"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Akce`. */
["AkcesOrderBy"]:AkcesOrderBy;
	/** A condition to be used against `Akce` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["AkceCondition"]: {
	/** Checks for equality with the object’s `aId` field. */
	aId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `aJmeno` field. */
	aJmeno?:string | null,
	/** Checks for equality with the object’s `aKde` field. */
	aKde?:string | null,
	/** Checks for equality with the object’s `aInfo` field. */
	aInfo?:string | null,
	/** Checks for equality with the object’s `aOd` field. */
	aOd?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `aDo` field. */
	aDo?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `aKapacita` field. */
	aKapacita?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `aDokumenty` field. */
	aDokumenty?:string | null,
	/** Checks for equality with the object’s `aTimestamp` field. */
	aTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `aLock` field. */
	aLock?:boolean | null,
	/** Checks for equality with the object’s `aVisible` field. */
	aVisible?:boolean | null
};
	/** A connection to a list of `GalerieDir` values. */
["GalerieDirsConnection"]: AliasType<{
	/** A list of `GalerieDir` objects. */
	nodes?:ValueTypes["GalerieDir"],
	/** A list of edges which contains the `GalerieDir` and cursor to aid in pagination. */
	edges?:ValueTypes["GalerieDirsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `GalerieDir` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `GalerieDir` edge in the connection. */
["GalerieDirsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `GalerieDir` at the end of the edge. */
	node?:ValueTypes["GalerieDir"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `GalerieDir`. */
["GalerieDirsOrderBy"]:GalerieDirsOrderBy;
	/** A condition to be used against `GalerieDir` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieDirCondition"]: {
	/** Checks for equality with the object’s `gdId` field. */
	gdId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `gdIdRodic` field. */
	gdIdRodic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `gdName` field. */
	gdName?:string | null,
	/** Checks for equality with the object’s `gdLevel` field. */
	gdLevel?:number | null,
	/** Checks for equality with the object’s `gdPath` field. */
	gdPath?:string | null,
	/** Checks for equality with the object’s `gdHidden` field. */
	gdHidden?:boolean | null
};
	/** A connection to a list of `Member` values. */
["MembersConnection"]: AliasType<{
	/** A list of `Member` objects. */
	nodes?:ValueTypes["Member"],
	/** A list of edges which contains the `Member` and cursor to aid in pagination. */
	edges?:ValueTypes["MembersEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Member` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Member"]: AliasType<{
	uId?:boolean,
	uLogin?:boolean,
	uPass?:boolean,
	uJmeno?:boolean,
	uPrijmeni?:boolean,
	uPohlavi?:boolean,
	uEmail?:boolean,
	uTelefon?:boolean,
	uNarozeni?:boolean,
	uRodneCislo?:boolean,
	uPoznamky?:boolean,
	uTimestamp?:boolean,
	uLevel?:boolean,
	uGroup?:boolean,
	uSkupina?:boolean,
	uDancer?:boolean,
	uBan?:boolean,
	uLock?:boolean,
	uConfirmed?:boolean,
	uSystem?:boolean,
	uStreet?:boolean,
	uConscriptionNumber?:boolean,
	uOrientationNumber?:boolean,
	uDistrict?:boolean,
	uCity?:boolean,
	uPostalCode?:boolean,
	uNationality?:boolean,
	uMemberSince?:boolean,
	uMemberUntil?:boolean,
	uCreatedAt?:boolean,
	uTeacher?:boolean,
	uGdprSignedAt?:boolean,
	sId?:boolean,
	sName?:boolean,
	paymentValid?:boolean,
		__typename?: boolean
}>;
	/** A `Member` edge in the connection. */
["MembersEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Member` at the end of the edge. */
	node?:ValueTypes["Member"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Member`. */
["MembersOrderBy"]:MembersOrderBy;
	/** A condition to be used against `Member` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["MemberCondition"]: {
	/** Checks for equality with the object’s `uId` field. */
	uId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uLogin` field. */
	uLogin?:string | null,
	/** Checks for equality with the object’s `uPass` field. */
	uPass?:string | null,
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?:string | null,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?:string | null,
	/** Checks for equality with the object’s `uPohlavi` field. */
	uPohlavi?:string | null,
	/** Checks for equality with the object’s `uEmail` field. */
	uEmail?:string | null,
	/** Checks for equality with the object’s `uTelefon` field. */
	uTelefon?:string | null,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `uRodneCislo` field. */
	uRodneCislo?:string | null,
	/** Checks for equality with the object’s `uPoznamky` field. */
	uPoznamky?:string | null,
	/** Checks for equality with the object’s `uTimestamp` field. */
	uTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uLevel` field. */
	uLevel?:number | null,
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uDancer` field. */
	uDancer?:boolean | null,
	/** Checks for equality with the object’s `uBan` field. */
	uBan?:boolean | null,
	/** Checks for equality with the object’s `uLock` field. */
	uLock?:boolean | null,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?:boolean | null,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?:boolean | null,
	/** Checks for equality with the object’s `uStreet` field. */
	uStreet?:string | null,
	/** Checks for equality with the object’s `uConscriptionNumber` field. */
	uConscriptionNumber?:string | null,
	/** Checks for equality with the object’s `uOrientationNumber` field. */
	uOrientationNumber?:string | null,
	/** Checks for equality with the object’s `uDistrict` field. */
	uDistrict?:string | null,
	/** Checks for equality with the object’s `uCity` field. */
	uCity?:string | null,
	/** Checks for equality with the object’s `uPostalCode` field. */
	uPostalCode?:string | null,
	/** Checks for equality with the object’s `uNationality` field. */
	uNationality?:string | null,
	/** Checks for equality with the object’s `uMemberSince` field. */
	uMemberSince?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uMemberUntil` field. */
	uMemberUntil?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uCreatedAt` field. */
	uCreatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `uTeacher` field. */
	uTeacher?:boolean | null,
	/** Checks for equality with the object’s `uGdprSignedAt` field. */
	uGdprSignedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `sId` field. */
	sId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `sName` field. */
	sName?:string | null,
	/** Checks for equality with the object’s `paymentValid` field. */
	paymentValid?:boolean | null
};
	/** A connection to a list of `Page` values. */
["PagesConnection"]: AliasType<{
	/** A list of `Page` objects. */
	nodes?:ValueTypes["Page"],
	/** A list of edges which contains the `Page` and cursor to aid in pagination. */
	edges?:ValueTypes["PagesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Page` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Page"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	id?:boolean,
	url?:boolean,
	content?:boolean,
	createdAt?:boolean,
	updatedAt?:boolean,
	title?:boolean,
		__typename?: boolean
}>;
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:unknown;
	/** A `Page` edge in the connection. */
["PagesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Page` at the end of the edge. */
	node?:ValueTypes["Page"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Page`. */
["PagesOrderBy"]:PagesOrderBy;
	/** A condition to be used against `Page` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PageCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:number | null,
	/** Checks for equality with the object’s `url` field. */
	url?:string | null,
	/** Checks for equality with the object’s `content` field. */
	content?:ValueTypes["JSON"] | null,
	/** Checks for equality with the object’s `createdAt` field. */
	createdAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `title` field. */
	title?:string | null
};
	/** A connection to a list of `PageRevision` values. */
["PageRevisionsConnection"]: AliasType<{
	/** A list of `PageRevision` objects. */
	nodes?:ValueTypes["PageRevision"],
	/** A list of edges which contains the `PageRevision` and cursor to aid in pagination. */
	edges?:ValueTypes["PageRevisionsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PageRevision` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["PageRevision"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	revNumber?:boolean,
	revOperation?:boolean,
	revTimestamp?:boolean,
	id?:boolean,
	url?:boolean,
	content?:boolean,
	createdAt?:boolean,
	updatedAt?:boolean,
	title?:boolean,
		__typename?: boolean
}>;
	/** A `PageRevision` edge in the connection. */
["PageRevisionsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PageRevision` at the end of the edge. */
	node?:ValueTypes["PageRevision"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PageRevision`. */
["PageRevisionsOrderBy"]:PageRevisionsOrderBy;
	/** A condition to be used against `PageRevision` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PageRevisionCondition"]: {
	/** Checks for equality with the object’s `revNumber` field. */
	revNumber?:number | null,
	/** Checks for equality with the object’s `revOperation` field. */
	revOperation?:string | null,
	/** Checks for equality with the object’s `revTimestamp` field. */
	revTimestamp?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `id` field. */
	id?:number | null,
	/** Checks for equality with the object’s `url` field. */
	url?:string | null,
	/** Checks for equality with the object’s `content` field. */
	content?:ValueTypes["JSON"] | null,
	/** Checks for equality with the object’s `createdAt` field. */
	createdAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `title` field. */
	title?:string | null
};
	/** A connection to a list of `Parameter` values. */
["ParametersConnection"]: AliasType<{
	/** A list of `Parameter` objects. */
	nodes?:ValueTypes["Parameter"],
	/** A list of edges which contains the `Parameter` and cursor to aid in pagination. */
	edges?:ValueTypes["ParametersEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Parameter` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Parameter"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	paName?:boolean,
	paValue?:boolean,
		__typename?: boolean
}>;
	/** A `Parameter` edge in the connection. */
["ParametersEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Parameter` at the end of the edge. */
	node?:ValueTypes["Parameter"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Parameter`. */
["ParametersOrderBy"]:ParametersOrderBy;
	/** A condition to be used against `Parameter` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParameterCondition"]: {
	/** Checks for equality with the object’s `paName` field. */
	paName?:string | null,
	/** Checks for equality with the object’s `paValue` field. */
	paValue?:string | null
};
	/** A connection to a list of `Permission` values. */
["PermissionsConnection"]: AliasType<{
	/** A list of `Permission` objects. */
	nodes?:ValueTypes["Permission"],
	/** A list of edges which contains the `Permission` and cursor to aid in pagination. */
	edges?:ValueTypes["PermissionsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Permission` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Permission` edge in the connection. */
["PermissionsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Permission` at the end of the edge. */
	node?:ValueTypes["Permission"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Permission`. */
["PermissionsOrderBy"]:PermissionsOrderBy;
	/** A condition to be used against `Permission` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PermissionCondition"]: {
	/** Checks for equality with the object’s `peId` field. */
	peId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `peName` field. */
	peName?:string | null,
	/** Checks for equality with the object’s `peDescription` field. */
	peDescription?:string | null,
	/** Checks for equality with the object’s `peAkce` field. */
	peAkce?:number | null,
	/** Checks for equality with the object’s `peAktuality` field. */
	peAktuality?:number | null,
	/** Checks for equality with the object’s `peAnkety` field. */
	peAnkety?:number | null,
	/** Checks for equality with the object’s `peDokumenty` field. */
	peDokumenty?:number | null,
	/** Checks for equality with the object’s `peGalerie` field. */
	peGalerie?:number | null,
	/** Checks for equality with the object’s `peInzerce` field. */
	peInzerce?:number | null,
	/** Checks for equality with the object’s `peKonzole` field. */
	peKonzole?:number | null,
	/** Checks for equality with the object’s `peNabidka` field. */
	peNabidka?:number | null,
	/** Checks for equality with the object’s `peNastenka` field. */
	peNastenka?:number | null,
	/** Checks for equality with the object’s `peNovinky` field. */
	peNovinky?:number | null,
	/** Checks for equality with the object’s `pePary` field. */
	pePary?:number | null,
	/** Checks for equality with the object’s `pePlatby` field. */
	pePlatby?:number | null,
	/** Checks for equality with the object’s `pePermissions` field. */
	pePermissions?:number | null,
	/** Checks for equality with the object’s `peRozpis` field. */
	peRozpis?:number | null,
	/** Checks for equality with the object’s `peSkupiny` field. */
	peSkupiny?:number | null,
	/** Checks for equality with the object’s `peUsers` field. */
	peUsers?:number | null,
	/** Checks for equality with the object’s `peMain` field. */
	peMain?:number | null
};
	/** A connection to a list of `PlatbyCategory` values. */
["PlatbyCategoriesConnection"]: AliasType<{
	/** A list of `PlatbyCategory` objects. */
	nodes?:ValueTypes["PlatbyCategory"],
	/** A list of edges which contains the `PlatbyCategory` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyCategoriesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyCategory` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `PlatbyCategory` edge in the connection. */
["PlatbyCategoriesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyCategory` at the end of the edge. */
	node?:ValueTypes["PlatbyCategory"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyCategory`. */
["PlatbyCategoriesOrderBy"]:PlatbyCategoriesOrderBy;
	/** A condition to be used against `PlatbyCategory` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryCondition"]: {
	/** Checks for equality with the object’s `pcId` field. */
	pcId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pcName` field. */
	pcName?:string | null,
	/** Checks for equality with the object’s `pcSymbol` field. */
	pcSymbol?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pcAmount` field. */
	pcAmount?:ValueTypes["BigFloat"] | null,
	/** Checks for equality with the object’s `pcDateDue` field. */
	pcDateDue?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `pcValidFrom` field. */
	pcValidFrom?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `pcValidTo` field. */
	pcValidTo?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `pcUseBase` field. */
	pcUseBase?:boolean | null,
	/** Checks for equality with the object’s `pcUsePrefix` field. */
	pcUsePrefix?:boolean | null,
	/** Checks for equality with the object’s `pcArchive` field. */
	pcArchive?:boolean | null,
	/** Checks for equality with the object’s `pcVisible` field. */
	pcVisible?:boolean | null
};
	/** A connection to a list of `PlatbyGroup` values. */
["PlatbyGroupsConnection"]: AliasType<{
	/** A list of `PlatbyGroup` objects. */
	nodes?:ValueTypes["PlatbyGroup"],
	/** A list of edges which contains the `PlatbyGroup` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyGroupsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyGroup` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `PlatbyGroup` edge in the connection. */
["PlatbyGroupsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyGroup` at the end of the edge. */
	node?:ValueTypes["PlatbyGroup"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyGroup`. */
["PlatbyGroupsOrderBy"]:PlatbyGroupsOrderBy;
	/** A condition to be used against `PlatbyGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyGroupCondition"]: {
	/** Checks for equality with the object’s `pgId` field. */
	pgId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `pgType` field. */
	pgType?:ValueTypes["BigFloat"] | null,
	/** Checks for equality with the object’s `pgName` field. */
	pgName?:string | null,
	/** Checks for equality with the object’s `pgDescription` field. */
	pgDescription?:string | null,
	/** Checks for equality with the object’s `pgBase` field. */
	pgBase?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `PlatbyRaw` values. */
["PlatbyRawsConnection"]: AliasType<{
	/** A list of `PlatbyRaw` objects. */
	nodes?:ValueTypes["PlatbyRaw"],
	/** A list of edges which contains the `PlatbyRaw` and cursor to aid in pagination. */
	edges?:ValueTypes["PlatbyRawsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `PlatbyRaw` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `PlatbyRaw` edge in the connection. */
["PlatbyRawsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `PlatbyRaw` at the end of the edge. */
	node?:ValueTypes["PlatbyRaw"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `PlatbyRaw`. */
["PlatbyRawsOrderBy"]:PlatbyRawsOrderBy;
	/** A condition to be used against `PlatbyRaw` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyRawCondition"]: {
	/** Checks for equality with the object’s `prId` field. */
	prId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `prRaw` field. */
	prRaw?:string | null,
	/** Checks for equality with the object’s `prHash` field. */
	prHash?:string | null,
	/** Checks for equality with the object’s `prSorted` field. */
	prSorted?:boolean | null,
	/** Checks for equality with the object’s `prDiscarded` field. */
	prDiscarded?:boolean | null
};
	/** A connection to a list of `Skupiny` values. */
["SkupiniesConnection"]: AliasType<{
	/** A list of `Skupiny` objects. */
	nodes?:ValueTypes["Skupiny"],
	/** A list of edges which contains the `Skupiny` and cursor to aid in pagination. */
	edges?:ValueTypes["SkupiniesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Skupiny` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Skupiny` edge in the connection. */
["SkupiniesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Skupiny` at the end of the edge. */
	node?:ValueTypes["Skupiny"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Skupiny`. */
["SkupiniesOrderBy"]:SkupiniesOrderBy;
	/** A condition to be used against `Skupiny` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SkupinyCondition"]: {
	/** Checks for equality with the object’s `sId` field. */
	sId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `sName` field. */
	sName?:string | null,
	/** Checks for equality with the object’s `sDescription` field. */
	sDescription?:string | null,
	/** Checks for equality with the object’s `sColorRgb` field. */
	sColorRgb?:string | null,
	/** Checks for equality with the object’s `sColorText` field. */
	sColorText?:string | null,
	/** Checks for equality with the object’s `sLocation` field. */
	sLocation?:string | null,
	/** Checks for equality with the object’s `sVisible` field. */
	sVisible?:boolean | null
};
	/** A connection to a list of `UsersSkupiny` values. */
["UsersSkupiniesConnection"]: AliasType<{
	/** A list of `UsersSkupiny` objects. */
	nodes?:ValueTypes["UsersSkupiny"],
	/** A list of edges which contains the `UsersSkupiny` and cursor to aid in pagination. */
	edges?:ValueTypes["UsersSkupiniesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `UsersSkupiny` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["UsersSkupiny"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	usId?:boolean,
	usColor?:boolean,
	usPlatbaMesic?:boolean,
	usPlatbaCtvrtrok?:boolean,
	usPlatbaPulrok?:boolean,
	usPopis?:boolean,
		__typename?: boolean
}>;
	/** A `UsersSkupiny` edge in the connection. */
["UsersSkupiniesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `UsersSkupiny` at the end of the edge. */
	node?:ValueTypes["UsersSkupiny"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `UsersSkupiny`. */
["UsersSkupiniesOrderBy"]:UsersSkupiniesOrderBy;
	/** A condition to be used against `UsersSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UsersSkupinyCondition"]: {
	/** Checks for equality with the object’s `usId` field. */
	usId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `usColor` field. */
	usColor?:string | null,
	/** Checks for equality with the object’s `usPlatbaMesic` field. */
	usPlatbaMesic?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `usPlatbaCtvrtrok` field. */
	usPlatbaCtvrtrok?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `usPlatbaPulrok` field. */
	usPlatbaPulrok?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `usPopis` field. */
	usPopis?:string | null
};
	/** A connection to a list of `Video` values. */
["VideosConnection"]: AliasType<{
	/** A list of `Video` objects. */
	nodes?:ValueTypes["Video"],
	/** A list of edges which contains the `Video` and cursor to aid in pagination. */
	edges?:ValueTypes["VideosEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Video` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Video"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	vId?:boolean,
	vUri?:boolean,
	vTitle?:boolean,
	vAuthor?:boolean,
	vDescription?:boolean,
	vPlaylist?:boolean,
	vCreatedAt?:boolean,
	vUpdatedAt?:boolean,
		__typename?: boolean
}>;
	/** A `Video` edge in the connection. */
["VideosEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Video` at the end of the edge. */
	node?:ValueTypes["Video"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Video`. */
["VideosOrderBy"]:VideosOrderBy;
	/** A condition to be used against `Video` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["VideoCondition"]: {
	/** Checks for equality with the object’s `vId` field. */
	vId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `vUri` field. */
	vUri?:string | null,
	/** Checks for equality with the object’s `vTitle` field. */
	vTitle?:string | null,
	/** Checks for equality with the object’s `vAuthor` field. */
	vAuthor?:string | null,
	/** Checks for equality with the object’s `vDescription` field. */
	vDescription?:string | null,
	/** Checks for equality with the object’s `vPlaylist` field. */
	vPlaylist?:string | null,
	/** Checks for equality with the object’s `vCreatedAt` field. */
	vCreatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `vUpdatedAt` field. */
	vUpdatedAt?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `VideoList` values. */
["VideoListsConnection"]: AliasType<{
	/** A list of `VideoList` objects. */
	nodes?:ValueTypes["VideoList"],
	/** A list of edges which contains the `VideoList` and cursor to aid in pagination. */
	edges?:ValueTypes["VideoListsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `VideoList` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["VideoList"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	vlId?:boolean,
	vlUrl?:boolean,
	vlTitle?:boolean,
	vlDescription?:boolean,
	vlCount?:boolean,
	vlCreatedAt?:boolean,
	vlLastChecked?:boolean,
		__typename?: boolean
}>;
	/** A `VideoList` edge in the connection. */
["VideoListsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `VideoList` at the end of the edge. */
	node?:ValueTypes["VideoList"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `VideoList`. */
["VideoListsOrderBy"]:VideoListsOrderBy;
	/** A condition to be used against `VideoList` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoListCondition"]: {
	/** Checks for equality with the object’s `vlId` field. */
	vlId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `vlUrl` field. */
	vlUrl?:string | null,
	/** Checks for equality with the object’s `vlTitle` field. */
	vlTitle?:string | null,
	/** Checks for equality with the object’s `vlDescription` field. */
	vlDescription?:string | null,
	/** Checks for equality with the object’s `vlCount` field. */
	vlCount?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `vlCreatedAt` field. */
	vlCreatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `vlLastChecked` field. */
	vlLastChecked?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `VideoSource` values. */
["VideoSourcesConnection"]: AliasType<{
	/** A list of `VideoSource` objects. */
	nodes?:ValueTypes["VideoSource"],
	/** A list of edges which contains the `VideoSource` and cursor to aid in pagination. */
	edges?:ValueTypes["VideoSourcesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `VideoSource` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["VideoSource"]: AliasType<{
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId?:boolean,
	vsId?:boolean,
	vsUrl?:boolean,
	vsTitle?:boolean,
	vsDescription?:boolean,
	vsCreatedAt?:boolean,
	vsLastChecked?:boolean,
		__typename?: boolean
}>;
	/** A `VideoSource` edge in the connection. */
["VideoSourcesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `VideoSource` at the end of the edge. */
	node?:ValueTypes["VideoSource"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `VideoSource`. */
["VideoSourcesOrderBy"]:VideoSourcesOrderBy;
	/** A condition to be used against `VideoSource` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoSourceCondition"]: {
	/** Checks for equality with the object’s `vsId` field. */
	vsId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `vsUrl` field. */
	vsUrl?:string | null,
	/** Checks for equality with the object’s `vsTitle` field. */
	vsTitle?:string | null,
	/** Checks for equality with the object’s `vsDescription` field. */
	vsDescription?:string | null,
	/** Checks for equality with the object’s `vsCreatedAt` field. */
	vsCreatedAt?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `vsLastChecked` field. */
	vsLastChecked?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `BigInt` values. */
["CurrentCoupleIdsConnection"]: AliasType<{
	/** A list of `BigInt` objects. */
	nodes?:boolean,
	/** A list of edges which contains the `BigInt` and cursor to aid in pagination. */
	edges?:ValueTypes["CurrentCoupleIdEdge"],
	/** The count of *all* `BigInt` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `BigInt` edge in the connection. */
["CurrentCoupleIdEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `BigInt` at the end of the edge. */
	node?:boolean,
		__typename?: boolean
}>;
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: AliasType<{
createAkce?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAkceInput"]},ValueTypes["CreateAkcePayload"]],
createAkceItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAkceItemInput"]},ValueTypes["CreateAkceItemPayload"]],
createAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAktualityInput"]},ValueTypes["CreateAktualityPayload"]],
createDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateDokumentyInput"]},ValueTypes["CreateDokumentyPayload"]],
createGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateGalerieDirInput"]},ValueTypes["CreateGalerieDirPayload"]],
createGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateGalerieFotoInput"]},ValueTypes["CreateGalerieFotoPayload"]],
createNabidka?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateNabidkaInput"]},ValueTypes["CreateNabidkaPayload"]],
createNabidkaItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateNabidkaItemInput"]},ValueTypes["CreateNabidkaItemPayload"]],
createPage?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePageInput"]},ValueTypes["CreatePagePayload"]],
createParameter?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateParameterInput"]},ValueTypes["CreateParameterPayload"]],
createPary?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateParyInput"]},ValueTypes["CreateParyPayload"]],
createParyNavrh?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateParyNavrhInput"]},ValueTypes["CreateParyNavrhPayload"]],
createPermission?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePermissionInput"]},ValueTypes["CreatePermissionPayload"]],
createPlatbyCategory?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyCategoryInput"]},ValueTypes["CreatePlatbyCategoryPayload"]],
createPlatbyCategoryGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyCategoryGroupInput"]},ValueTypes["CreatePlatbyCategoryGroupPayload"]],
createPlatbyGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyGroupInput"]},ValueTypes["CreatePlatbyGroupPayload"]],
createPlatbyGroupSkupina?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyGroupSkupinaInput"]},ValueTypes["CreatePlatbyGroupSkupinaPayload"]],
createPlatbyItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyItemInput"]},ValueTypes["CreatePlatbyItemPayload"]],
createPlatbyRaw?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePlatbyRawInput"]},ValueTypes["CreatePlatbyRawPayload"]],
createRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRozpiInput"]},ValueTypes["CreateRozpiPayload"]],
createRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRozpisItemInput"]},ValueTypes["CreateRozpisItemPayload"]],
createSession?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateSessionInput"]},ValueTypes["CreateSessionPayload"]],
createSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateSkupinyInput"]},ValueTypes["CreateSkupinyPayload"]],
createUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUpozorneniInput"]},ValueTypes["CreateUpozorneniPayload"]],
createUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUpozorneniSkupinyInput"]},ValueTypes["CreateUpozorneniSkupinyPayload"]],
createUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUserInput"]},ValueTypes["CreateUserPayload"]],
createUsersSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUsersSkupinyInput"]},ValueTypes["CreateUsersSkupinyPayload"]],
createVideo?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateVideoInput"]},ValueTypes["CreateVideoPayload"]],
createVideoList?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateVideoListInput"]},ValueTypes["CreateVideoListPayload"]],
createVideoSource?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateVideoSourceInput"]},ValueTypes["CreateVideoSourcePayload"]],
updateAkceByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAkceByNodeIdInput"]},ValueTypes["UpdateAkcePayload"]],
updateAkce?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAkceInput"]},ValueTypes["UpdateAkcePayload"]],
updateAkceItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAkceItemByNodeIdInput"]},ValueTypes["UpdateAkceItemPayload"]],
updateAkceItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAkceItemInput"]},ValueTypes["UpdateAkceItemPayload"]],
updateAktualityByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAktualityByNodeIdInput"]},ValueTypes["UpdateAktualityPayload"]],
updateAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAktualityInput"]},ValueTypes["UpdateAktualityPayload"]],
updateDokumentyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateDokumentyByNodeIdInput"]},ValueTypes["UpdateDokumentyPayload"]],
updateDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateDokumentyInput"]},ValueTypes["UpdateDokumentyPayload"]],
updateGalerieDirByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieDirByNodeIdInput"]},ValueTypes["UpdateGalerieDirPayload"]],
updateGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieDirInput"]},ValueTypes["UpdateGalerieDirPayload"]],
updateGalerieFotoByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieFotoByNodeIdInput"]},ValueTypes["UpdateGalerieFotoPayload"]],
updateGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieFotoInput"]},ValueTypes["UpdateGalerieFotoPayload"]],
updateNabidkaByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaByNodeIdInput"]},ValueTypes["UpdateNabidkaPayload"]],
updateNabidka?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaInput"]},ValueTypes["UpdateNabidkaPayload"]],
updateNabidkaItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaItemByNodeIdInput"]},ValueTypes["UpdateNabidkaItemPayload"]],
updateNabidkaItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaItemInput"]},ValueTypes["UpdateNabidkaItemPayload"]],
updatePageByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePageByNodeIdInput"]},ValueTypes["UpdatePagePayload"]],
updatePage?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePageInput"]},ValueTypes["UpdatePagePayload"]],
updatePageByUrl?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePageByUrlInput"]},ValueTypes["UpdatePagePayload"]],
updateParameterByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParameterByNodeIdInput"]},ValueTypes["UpdateParameterPayload"]],
updateParameter?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParameterInput"]},ValueTypes["UpdateParameterPayload"]],
updateParyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyByNodeIdInput"]},ValueTypes["UpdateParyPayload"]],
updatePary?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyInput"]},ValueTypes["UpdateParyPayload"]],
updateParyNavrhByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyNavrhByNodeIdInput"]},ValueTypes["UpdateParyNavrhPayload"]],
updateParyNavrh?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyNavrhInput"]},ValueTypes["UpdateParyNavrhPayload"]],
updatePermissionByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePermissionByNodeIdInput"]},ValueTypes["UpdatePermissionPayload"]],
updatePermission?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePermissionInput"]},ValueTypes["UpdatePermissionPayload"]],
updatePlatbyCategoryByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryByNodeIdInput"]},ValueTypes["UpdatePlatbyCategoryPayload"]],
updatePlatbyCategory?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryInput"]},ValueTypes["UpdatePlatbyCategoryPayload"]],
updatePlatbyCategoryGroupByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryGroupByNodeIdInput"]},ValueTypes["UpdatePlatbyCategoryGroupPayload"]],
updatePlatbyCategoryGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryGroupInput"]},ValueTypes["UpdatePlatbyCategoryGroupPayload"]],
updatePlatbyGroupByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupByNodeIdInput"]},ValueTypes["UpdatePlatbyGroupPayload"]],
updatePlatbyGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupInput"]},ValueTypes["UpdatePlatbyGroupPayload"]],
updatePlatbyGroupSkupinaByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupSkupinaByNodeIdInput"]},ValueTypes["UpdatePlatbyGroupSkupinaPayload"]],
updatePlatbyGroupSkupina?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupSkupinaInput"]},ValueTypes["UpdatePlatbyGroupSkupinaPayload"]],
updatePlatbyItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyItemByNodeIdInput"]},ValueTypes["UpdatePlatbyItemPayload"]],
updatePlatbyItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyItemInput"]},ValueTypes["UpdatePlatbyItemPayload"]],
updatePlatbyRawByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyRawByNodeIdInput"]},ValueTypes["UpdatePlatbyRawPayload"]],
updatePlatbyRaw?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyRawInput"]},ValueTypes["UpdatePlatbyRawPayload"]],
updateRozpiByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpiByNodeIdInput"]},ValueTypes["UpdateRozpiPayload"]],
updateRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpiInput"]},ValueTypes["UpdateRozpiPayload"]],
updateRozpisItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpisItemByNodeIdInput"]},ValueTypes["UpdateRozpisItemPayload"]],
updateRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpisItemInput"]},ValueTypes["UpdateRozpisItemPayload"]],
updateSessionByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateSessionByNodeIdInput"]},ValueTypes["UpdateSessionPayload"]],
updateSession?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateSessionInput"]},ValueTypes["UpdateSessionPayload"]],
updateSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateSkupinyByNodeIdInput"]},ValueTypes["UpdateSkupinyPayload"]],
updateSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateSkupinyInput"]},ValueTypes["UpdateSkupinyPayload"]],
updateUpozorneniByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniByNodeIdInput"]},ValueTypes["UpdateUpozorneniPayload"]],
updateUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniInput"]},ValueTypes["UpdateUpozorneniPayload"]],
updateUpozorneniSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniSkupinyByNodeIdInput"]},ValueTypes["UpdateUpozorneniSkupinyPayload"]],
updateUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniSkupinyInput"]},ValueTypes["UpdateUpozorneniSkupinyPayload"]],
updateUserByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUserByNodeIdInput"]},ValueTypes["UpdateUserPayload"]],
updateUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUserInput"]},ValueTypes["UpdateUserPayload"]],
updateUsersSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUsersSkupinyByNodeIdInput"]},ValueTypes["UpdateUsersSkupinyPayload"]],
updateUsersSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUsersSkupinyInput"]},ValueTypes["UpdateUsersSkupinyPayload"]],
updateVideoByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoByNodeIdInput"]},ValueTypes["UpdateVideoPayload"]],
updateVideo?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoInput"]},ValueTypes["UpdateVideoPayload"]],
updateVideoListByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoListByNodeIdInput"]},ValueTypes["UpdateVideoListPayload"]],
updateVideoList?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoListInput"]},ValueTypes["UpdateVideoListPayload"]],
updateVideoSourceByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoSourceByNodeIdInput"]},ValueTypes["UpdateVideoSourcePayload"]],
updateVideoSource?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateVideoSourceInput"]},ValueTypes["UpdateVideoSourcePayload"]],
deleteAkceByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAkceByNodeIdInput"]},ValueTypes["DeleteAkcePayload"]],
deleteAkce?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAkceInput"]},ValueTypes["DeleteAkcePayload"]],
deleteAkceItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAkceItemByNodeIdInput"]},ValueTypes["DeleteAkceItemPayload"]],
deleteAkceItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAkceItemInput"]},ValueTypes["DeleteAkceItemPayload"]],
deleteAktualityByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAktualityByNodeIdInput"]},ValueTypes["DeleteAktualityPayload"]],
deleteAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAktualityInput"]},ValueTypes["DeleteAktualityPayload"]],
deleteDokumentyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteDokumentyByNodeIdInput"]},ValueTypes["DeleteDokumentyPayload"]],
deleteDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteDokumentyInput"]},ValueTypes["DeleteDokumentyPayload"]],
deleteGalerieDirByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieDirByNodeIdInput"]},ValueTypes["DeleteGalerieDirPayload"]],
deleteGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieDirInput"]},ValueTypes["DeleteGalerieDirPayload"]],
deleteGalerieFotoByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieFotoByNodeIdInput"]},ValueTypes["DeleteGalerieFotoPayload"]],
deleteGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieFotoInput"]},ValueTypes["DeleteGalerieFotoPayload"]],
deleteNabidkaByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaByNodeIdInput"]},ValueTypes["DeleteNabidkaPayload"]],
deleteNabidka?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaInput"]},ValueTypes["DeleteNabidkaPayload"]],
deleteNabidkaItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaItemByNodeIdInput"]},ValueTypes["DeleteNabidkaItemPayload"]],
deleteNabidkaItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaItemInput"]},ValueTypes["DeleteNabidkaItemPayload"]],
deleteParameterByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParameterByNodeIdInput"]},ValueTypes["DeleteParameterPayload"]],
deleteParameter?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParameterInput"]},ValueTypes["DeleteParameterPayload"]],
deleteParyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyByNodeIdInput"]},ValueTypes["DeleteParyPayload"]],
deletePary?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyInput"]},ValueTypes["DeleteParyPayload"]],
deleteParyNavrhByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyNavrhByNodeIdInput"]},ValueTypes["DeleteParyNavrhPayload"]],
deleteParyNavrh?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyNavrhInput"]},ValueTypes["DeleteParyNavrhPayload"]],
deletePermissionByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePermissionByNodeIdInput"]},ValueTypes["DeletePermissionPayload"]],
deletePermission?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePermissionInput"]},ValueTypes["DeletePermissionPayload"]],
deletePlatbyCategoryByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryByNodeIdInput"]},ValueTypes["DeletePlatbyCategoryPayload"]],
deletePlatbyCategory?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryInput"]},ValueTypes["DeletePlatbyCategoryPayload"]],
deletePlatbyCategoryGroupByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryGroupByNodeIdInput"]},ValueTypes["DeletePlatbyCategoryGroupPayload"]],
deletePlatbyCategoryGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryGroupInput"]},ValueTypes["DeletePlatbyCategoryGroupPayload"]],
deletePlatbyGroupByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupByNodeIdInput"]},ValueTypes["DeletePlatbyGroupPayload"]],
deletePlatbyGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupInput"]},ValueTypes["DeletePlatbyGroupPayload"]],
deletePlatbyGroupSkupinaByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupSkupinaByNodeIdInput"]},ValueTypes["DeletePlatbyGroupSkupinaPayload"]],
deletePlatbyGroupSkupina?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupSkupinaInput"]},ValueTypes["DeletePlatbyGroupSkupinaPayload"]],
deletePlatbyItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyItemByNodeIdInput"]},ValueTypes["DeletePlatbyItemPayload"]],
deletePlatbyItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyItemInput"]},ValueTypes["DeletePlatbyItemPayload"]],
deletePlatbyRawByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyRawByNodeIdInput"]},ValueTypes["DeletePlatbyRawPayload"]],
deletePlatbyRaw?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyRawInput"]},ValueTypes["DeletePlatbyRawPayload"]],
deleteRozpiByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpiByNodeIdInput"]},ValueTypes["DeleteRozpiPayload"]],
deleteRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpiInput"]},ValueTypes["DeleteRozpiPayload"]],
deleteRozpisItemByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpisItemByNodeIdInput"]},ValueTypes["DeleteRozpisItemPayload"]],
deleteRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpisItemInput"]},ValueTypes["DeleteRozpisItemPayload"]],
deleteSessionByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteSessionByNodeIdInput"]},ValueTypes["DeleteSessionPayload"]],
deleteSession?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteSessionInput"]},ValueTypes["DeleteSessionPayload"]],
deleteSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteSkupinyByNodeIdInput"]},ValueTypes["DeleteSkupinyPayload"]],
deleteSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteSkupinyInput"]},ValueTypes["DeleteSkupinyPayload"]],
deleteUpozorneniByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniByNodeIdInput"]},ValueTypes["DeleteUpozorneniPayload"]],
deleteUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniInput"]},ValueTypes["DeleteUpozorneniPayload"]],
deleteUpozorneniSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniSkupinyByNodeIdInput"]},ValueTypes["DeleteUpozorneniSkupinyPayload"]],
deleteUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniSkupinyInput"]},ValueTypes["DeleteUpozorneniSkupinyPayload"]],
deleteUserByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUserByNodeIdInput"]},ValueTypes["DeleteUserPayload"]],
deleteUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUserInput"]},ValueTypes["DeleteUserPayload"]],
deleteUsersSkupinyByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUsersSkupinyByNodeIdInput"]},ValueTypes["DeleteUsersSkupinyPayload"]],
deleteUsersSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUsersSkupinyInput"]},ValueTypes["DeleteUsersSkupinyPayload"]],
deleteVideoByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoByNodeIdInput"]},ValueTypes["DeleteVideoPayload"]],
deleteVideo?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoInput"]},ValueTypes["DeleteVideoPayload"]],
deleteVideoListByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoListByNodeIdInput"]},ValueTypes["DeleteVideoListPayload"]],
deleteVideoList?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoListInput"]},ValueTypes["DeleteVideoListPayload"]],
deleteVideoSourceByNodeId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoSourceByNodeIdInput"]},ValueTypes["DeleteVideoSourcePayload"]],
deleteVideoSource?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteVideoSourceInput"]},ValueTypes["DeleteVideoSourcePayload"]],
login?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["LoginInput"]},ValueTypes["LoginPayload"]],
logout?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["LogoutInput"]},ValueTypes["LogoutPayload"]],
uploadFile?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UploadInput"]},ValueTypes["Upload"]],
		__typename?: boolean
}>;
	/** The output of our create `Akce` mutation. */
["CreateAkcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Akce` that was created by this mutation. */
	akce?:ValueTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
akceEdge?: [{	/** The method to use when ordering `Akce`. */
	orderBy?:ValueTypes["AkcesOrderBy"][]},ValueTypes["AkcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Akce` mutation. */
["CreateAkceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Akce` to be created by this mutation. */
	akce:ValueTypes["AkceInput"]
};
	/** An input for mutations affecting `Akce` */
["AkceInput"]: {
	aId?:ValueTypes["BigInt"] | null,
	aJmeno:string,
	aKde:string,
	aInfo:string,
	aOd:ValueTypes["Date"],
	aDo:ValueTypes["Date"],
	aKapacita?:ValueTypes["BigInt"] | null,
	aDokumenty:string,
	aTimestamp?:ValueTypes["Datetime"] | null,
	aLock?:boolean | null,
	aVisible?:boolean | null
};
	/** The output of our create `AkceItem` mutation. */
["CreateAkceItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AkceItem` that was created by this mutation. */
	akceItem?:ValueTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ValueTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ValueTypes["User"],
akceItemEdge?: [{	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `AkceItem` mutation. */
["CreateAkceItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `AkceItem` to be created by this mutation. */
	akceItem:ValueTypes["AkceItemInput"]
};
	/** An input for mutations affecting `AkceItem` */
["AkceItemInput"]: {
	aiId?:ValueTypes["BigInt"] | null,
	aiIdRodic:ValueTypes["BigInt"],
	aiUser:ValueTypes["BigInt"],
	aiRokNarozeni:number
};
	/** The output of our create `Aktuality` mutation. */
["CreateAktualityPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Aktuality` that was created by this mutation. */
	aktuality?:ValueTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ValueTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ValueTypes["GalerieFoto"],
aktualityEdge?: [{	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][]},ValueTypes["AktualitiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Aktuality` mutation. */
["CreateAktualityInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Aktuality` to be created by this mutation. */
	aktuality:ValueTypes["AktualityInput"]
};
	/** An input for mutations affecting `Aktuality` */
["AktualityInput"]: {
	atId?:ValueTypes["BigInt"] | null,
	atKdo:ValueTypes["BigInt"],
	atKat:string,
	atJmeno:string,
	atText:string,
	atPreview:string,
	atFoto?:ValueTypes["BigInt"] | null,
	atFotoMain?:ValueTypes["BigInt"] | null,
	atTimestamp?:ValueTypes["Datetime"] | null,
	atTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** The output of our create `Dokumenty` mutation. */
["CreateDokumentyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Dokumenty` that was created by this mutation. */
	dokumenty?:ValueTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ValueTypes["User"],
dokumentyEdge?: [{	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][]},ValueTypes["DokumentiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Dokumenty` mutation. */
["CreateDokumentyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Dokumenty` to be created by this mutation. */
	dokumenty:ValueTypes["DokumentyInput"]
};
	/** An input for mutations affecting `Dokumenty` */
["DokumentyInput"]: {
	dId?:ValueTypes["BigInt"] | null,
	dPath:string,
	dName:string,
	dFilename:string,
	dKategorie:number,
	dKdo:ValueTypes["BigInt"],
	dTimestamp?:ValueTypes["Datetime"] | null
};
	/** The output of our create `GalerieDir` mutation. */
["CreateGalerieDirPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieDir` that was created by this mutation. */
	galerieDir?:ValueTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
galerieDirEdge?: [{	/** The method to use when ordering `GalerieDir`. */
	orderBy?:ValueTypes["GalerieDirsOrderBy"][]},ValueTypes["GalerieDirsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `GalerieDir` mutation. */
["CreateGalerieDirInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `GalerieDir` to be created by this mutation. */
	galerieDir:ValueTypes["GalerieDirInput"]
};
	/** An input for mutations affecting `GalerieDir` */
["GalerieDirInput"]: {
	gdId?:ValueTypes["BigInt"] | null,
	gdIdRodic:ValueTypes["BigInt"],
	gdName:string,
	gdLevel?:number | null,
	gdPath:string,
	gdHidden?:boolean | null
};
	/** The output of our create `GalerieFoto` mutation. */
["CreateGalerieFotoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieFoto` that was created by this mutation. */
	galerieFoto?:ValueTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ValueTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ValueTypes["User"],
galerieFotoEdge?: [{	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][]},ValueTypes["GalerieFotosEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `GalerieFoto` mutation. */
["CreateGalerieFotoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `GalerieFoto` to be created by this mutation. */
	galerieFoto:ValueTypes["GalerieFotoInput"]
};
	/** An input for mutations affecting `GalerieFoto` */
["GalerieFotoInput"]: {
	gfId?:ValueTypes["BigInt"] | null,
	gfIdRodic:ValueTypes["BigInt"],
	gfName:string,
	gfPath:string,
	gfKdo:ValueTypes["BigInt"],
	gfTimestamp?:ValueTypes["Datetime"] | null
};
	/** The output of our create `Nabidka` mutation. */
["CreateNabidkaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Nabidka` that was created by this mutation. */
	nabidka?:ValueTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ValueTypes["User"],
nabidkaEdge?: [{	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][]},ValueTypes["NabidkasEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Nabidka` mutation. */
["CreateNabidkaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Nabidka` to be created by this mutation. */
	nabidka:ValueTypes["NabidkaInput"]
};
	/** An input for mutations affecting `Nabidka` */
["NabidkaInput"]: {
	nId?:ValueTypes["BigInt"] | null,
	nTrener:ValueTypes["BigInt"],
	nPocetHod?:number | null,
	nMaxPocetHod?:ValueTypes["BigInt"] | null,
	nOd:ValueTypes["Date"],
	nDo:ValueTypes["Date"],
	nVisible?:boolean | null,
	nLock?:boolean | null,
	nTimestamp?:ValueTypes["Datetime"] | null
};
	/** The output of our create `NabidkaItem` mutation. */
["CreateNabidkaItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `NabidkaItem` that was created by this mutation. */
	nabidkaItem?:ValueTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ValueTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ValueTypes["Pary"],
nabidkaItemEdge?: [{	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][]},ValueTypes["NabidkaItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `NabidkaItem` mutation. */
["CreateNabidkaItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `NabidkaItem` to be created by this mutation. */
	nabidkaItem:ValueTypes["NabidkaItemInput"]
};
	/** An input for mutations affecting `NabidkaItem` */
["NabidkaItemInput"]: {
	niId?:ValueTypes["BigInt"] | null,
	niIdRodic:ValueTypes["BigInt"],
	niPartner:ValueTypes["BigInt"],
	niPocetHod?:number | null,
	niLock?:boolean | null
};
	/** The output of our create `Page` mutation. */
["CreatePagePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Page` that was created by this mutation. */
	page?:ValueTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
pageEdge?: [{	/** The method to use when ordering `Page`. */
	orderBy?:ValueTypes["PagesOrderBy"][]},ValueTypes["PagesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Page` mutation. */
["CreatePageInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Page` to be created by this mutation. */
	page:ValueTypes["PageInput"]
};
	/** An input for mutations affecting `Page` */
["PageInput"]: {
	id?:number | null,
	url:string,
	content:ValueTypes["JSON"],
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	title?:string | null
};
	/** The output of our create `Parameter` mutation. */
["CreateParameterPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Parameter` that was created by this mutation. */
	parameter?:ValueTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
parameterEdge?: [{	/** The method to use when ordering `Parameter`. */
	orderBy?:ValueTypes["ParametersOrderBy"][]},ValueTypes["ParametersEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Parameter` mutation. */
["CreateParameterInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Parameter` to be created by this mutation. */
	parameter:ValueTypes["ParameterInput"]
};
	/** An input for mutations affecting `Parameter` */
["ParameterInput"]: {
	paName:string,
	paValue:string
};
	/** The output of our create `Pary` mutation. */
["CreateParyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Pary` that was created by this mutation. */
	pary?:ValueTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ValueTypes["User"],
paryEdge?: [{	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][]},ValueTypes["PariesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Pary` mutation. */
["CreateParyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Pary` to be created by this mutation. */
	pary:ValueTypes["ParyInput"]
};
	/** An input for mutations affecting `Pary` */
["ParyInput"]: {
	pId?:ValueTypes["BigInt"] | null,
	pIdPartner:ValueTypes["BigInt"],
	pIdPartnerka?:ValueTypes["BigInt"] | null,
	pSttTrida?:ValueTypes["ParyPSttTrida"] | null,
	pSttBody?:number | null,
	pSttFinale?:boolean | null,
	pLatTrida?:ValueTypes["ParyPLatTrida"] | null,
	pLatBody?:number | null,
	pLatFinale?:boolean | null,
	pHodnoceni?:number | null,
	pArchiv?:boolean | null,
	pTimestampAdd?:ValueTypes["Datetime"] | null,
	pTimestampArchive?:ValueTypes["Datetime"] | null
};
	/** The output of our create `ParyNavrh` mutation. */
["CreateParyNavrhPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `ParyNavrh` that was created by this mutation. */
	paryNavrh?:ValueTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ValueTypes["User"],
paryNavrhEdge?: [{	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][]},ValueTypes["ParyNavrhsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `ParyNavrh` mutation. */
["CreateParyNavrhInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `ParyNavrh` to be created by this mutation. */
	paryNavrh:ValueTypes["ParyNavrhInput"]
};
	/** An input for mutations affecting `ParyNavrh` */
["ParyNavrhInput"]: {
	pnId?:ValueTypes["BigInt"] | null,
	pnNavrhl:ValueTypes["BigInt"],
	pnPartner:ValueTypes["BigInt"],
	pnPartnerka:ValueTypes["BigInt"]
};
	/** The output of our create `Permission` mutation. */
["CreatePermissionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Permission` that was created by this mutation. */
	permission?:ValueTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
permissionEdge?: [{	/** The method to use when ordering `Permission`. */
	orderBy?:ValueTypes["PermissionsOrderBy"][]},ValueTypes["PermissionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Permission` mutation. */
["CreatePermissionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Permission` to be created by this mutation. */
	permission:ValueTypes["PermissionInput"]
};
	/** An input for mutations affecting `Permission` */
["PermissionInput"]: {
	peId?:ValueTypes["BigInt"] | null,
	peName:string,
	peDescription:string,
	peAkce:number,
	peAktuality:number,
	peAnkety:number,
	peDokumenty:number,
	peGalerie:number,
	peInzerce:number,
	peKonzole:number,
	peNabidka:number,
	peNastenka:number,
	peNovinky:number,
	pePary:number,
	pePlatby:number,
	pePermissions:number,
	peRozpis:number,
	peSkupiny:number,
	peUsers:number,
	peMain:number
};
	/** The output of our create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategory` that was created by this mutation. */
	platbyCategory?:ValueTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyCategoryEdge?: [{	/** The method to use when ordering `PlatbyCategory`. */
	orderBy?:ValueTypes["PlatbyCategoriesOrderBy"][]},ValueTypes["PlatbyCategoriesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyCategory` to be created by this mutation. */
	platbyCategory:ValueTypes["PlatbyCategoryInput"]
};
	/** An input for mutations affecting `PlatbyCategory` */
["PlatbyCategoryInput"]: {
	pcId?:ValueTypes["BigInt"] | null,
	pcName:string,
	pcSymbol:ValueTypes["BigInt"],
	pcAmount:ValueTypes["BigFloat"],
	pcDateDue:ValueTypes["Date"],
	pcValidFrom:ValueTypes["Date"],
	pcValidTo:ValueTypes["Date"],
	pcUseBase?:boolean | null,
	pcUsePrefix?:boolean | null,
	pcArchive?:boolean | null,
	pcVisible?:boolean | null
};
	/** The output of our create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategoryGroup` that was created by this mutation. */
	platbyCategoryGroup?:ValueTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ValueTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ValueTypes["PlatbyCategory"],
platbyCategoryGroupEdge?: [{	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][]},ValueTypes["PlatbyCategoryGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyCategoryGroup` to be created by this mutation. */
	platbyCategoryGroup:ValueTypes["PlatbyCategoryGroupInput"]
};
	/** An input for mutations affecting `PlatbyCategoryGroup` */
["PlatbyCategoryGroupInput"]: {
	pcgId?:ValueTypes["BigInt"] | null,
	pcgIdGroup:ValueTypes["BigInt"],
	pcgIdCategory:ValueTypes["BigInt"]
};
	/** The output of our create `PlatbyGroup` mutation. */
["CreatePlatbyGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroup` that was created by this mutation. */
	platbyGroup?:ValueTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyGroupEdge?: [{	/** The method to use when ordering `PlatbyGroup`. */
	orderBy?:ValueTypes["PlatbyGroupsOrderBy"][]},ValueTypes["PlatbyGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyGroup` mutation. */
["CreatePlatbyGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyGroup` to be created by this mutation. */
	platbyGroup:ValueTypes["PlatbyGroupInput"]
};
	/** An input for mutations affecting `PlatbyGroup` */
["PlatbyGroupInput"]: {
	pgId?:ValueTypes["BigInt"] | null,
	pgType?:ValueTypes["BigFloat"] | null,
	pgName:string,
	pgDescription:string,
	pgBase?:ValueTypes["BigInt"] | null
};
	/** The output of our create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroupSkupina` that was created by this mutation. */
	platbyGroupSkupina?:ValueTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ValueTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ValueTypes["PlatbyGroup"],
platbyGroupSkupinaEdge?: [{	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][]},ValueTypes["PlatbyGroupSkupinasEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyGroupSkupina` to be created by this mutation. */
	platbyGroupSkupina:ValueTypes["PlatbyGroupSkupinaInput"]
};
	/** An input for mutations affecting `PlatbyGroupSkupina` */
["PlatbyGroupSkupinaInput"]: {
	pgsId?:ValueTypes["BigInt"] | null,
	pgsIdSkupina:ValueTypes["BigInt"],
	pgsIdGroup:ValueTypes["BigInt"]
};
	/** The output of our create `PlatbyItem` mutation. */
["CreatePlatbyItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyItem` that was created by this mutation. */
	platbyItem?:ValueTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ValueTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ValueTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ValueTypes["PlatbyRaw"],
platbyItemEdge?: [{	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][]},ValueTypes["PlatbyItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyItem` mutation. */
["CreatePlatbyItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyItem` to be created by this mutation. */
	platbyItem:ValueTypes["PlatbyItemInput"]
};
	/** An input for mutations affecting `PlatbyItem` */
["PlatbyItemInput"]: {
	piId?:ValueTypes["BigInt"] | null,
	piIdUser?:ValueTypes["BigInt"] | null,
	piIdCategory:ValueTypes["BigInt"],
	piIdRaw?:ValueTypes["BigInt"] | null,
	piAmount:ValueTypes["BigFloat"],
	piDate:ValueTypes["Date"],
	piPrefix?:number | null
};
	/** The output of our create `PlatbyRaw` mutation. */
["CreatePlatbyRawPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyRaw` that was created by this mutation. */
	platbyRaw?:ValueTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyRawEdge?: [{	/** The method to use when ordering `PlatbyRaw`. */
	orderBy?:ValueTypes["PlatbyRawsOrderBy"][]},ValueTypes["PlatbyRawsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `PlatbyRaw` mutation. */
["CreatePlatbyRawInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `PlatbyRaw` to be created by this mutation. */
	platbyRaw:ValueTypes["PlatbyRawInput"]
};
	/** An input for mutations affecting `PlatbyRaw` */
["PlatbyRawInput"]: {
	prId?:ValueTypes["BigInt"] | null,
	prRaw:string,
	prHash:string,
	prSorted?:boolean | null,
	prDiscarded?:boolean | null
};
	/** The output of our create `Rozpi` mutation. */
["CreateRozpiPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Rozpi` that was created by this mutation. */
	rozpi?:ValueTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ValueTypes["User"],
rozpiEdge?: [{	/** The method to use when ordering `Rozpi`. */
	orderBy?:ValueTypes["RozpisOrderBy"][]},ValueTypes["RozpisEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Rozpi` mutation. */
["CreateRozpiInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Rozpi` to be created by this mutation. */
	rozpi:ValueTypes["RozpiInput"]
};
	/** An input for mutations affecting `Rozpi` */
["RozpiInput"]: {
	rId?:ValueTypes["BigInt"] | null,
	rTrener:ValueTypes["BigInt"],
	rKde:string,
	rDatum:ValueTypes["Date"],
	rVisible?:boolean | null,
	rLock?:boolean | null,
	rTimestamp?:ValueTypes["Datetime"] | null
};
	/** The output of our create `RozpisItem` mutation. */
["CreateRozpisItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RozpisItem` that was created by this mutation. */
	rozpisItem?:ValueTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ValueTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ValueTypes["Pary"],
rozpisItemEdge?: [{	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][]},ValueTypes["RozpisItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `RozpisItem` mutation. */
["CreateRozpisItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `RozpisItem` to be created by this mutation. */
	rozpisItem:ValueTypes["RozpisItemInput"]
};
	/** An input for mutations affecting `RozpisItem` */
["RozpisItemInput"]: {
	riId?:ValueTypes["BigInt"] | null,
	riIdRodic:ValueTypes["BigInt"],
	riPartner?:ValueTypes["BigInt"] | null,
	riOd:ValueTypes["Time"],
	riDo:ValueTypes["Time"],
	riLock?:boolean | null
};
	/** The output of our create `Session` mutation. */
["CreateSessionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Session` that was created by this mutation. */
	session?:ValueTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ValueTypes["User"],
sessionEdge?: [{	/** The method to use when ordering `Session`. */
	orderBy?:ValueTypes["SessionsOrderBy"][]},ValueTypes["SessionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Session` mutation. */
["CreateSessionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Session` to be created by this mutation. */
	session:ValueTypes["SessionInput"]
};
	/** An input for mutations affecting `Session` */
["SessionInput"]: {
	ssId:string,
	ssData:string,
	ssUpdatedAt?:ValueTypes["Datetime"] | null,
	ssLifetime:ValueTypes["BigInt"],
	ssUser?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Skupiny` mutation. */
["CreateSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Skupiny` that was created by this mutation. */
	skupiny?:ValueTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
skupinyEdge?: [{	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][]},ValueTypes["SkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Skupiny` mutation. */
["CreateSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Skupiny` to be created by this mutation. */
	skupiny:ValueTypes["SkupinyInput"]
};
	/** An input for mutations affecting `Skupiny` */
["SkupinyInput"]: {
	sId?:ValueTypes["BigInt"] | null,
	sName:string,
	sDescription:string,
	sColorRgb:string,
	sColorText:string,
	sLocation?:string | null,
	sVisible?:boolean | null
};
	/** The output of our create `Upozorneni` mutation. */
["CreateUpozorneniPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Upozorneni` that was created by this mutation. */
	upozorneni?:ValueTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ValueTypes["User"],
upozorneniEdge?: [{	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][]},ValueTypes["UpozornenisEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Upozorneni` mutation. */
["CreateUpozorneniInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Upozorneni` to be created by this mutation. */
	upozorneni:ValueTypes["UpozorneniInput"]
};
	/** An input for mutations affecting `Upozorneni` */
["UpozorneniInput"]: {
	upId?:ValueTypes["BigInt"] | null,
	upKdo:ValueTypes["BigInt"],
	upNadpis:string,
	upText:string,
	upBarvy?:ValueTypes["BigInt"] | null,
	upLock?:boolean | null,
	upTimestamp?:ValueTypes["Datetime"] | null,
	upTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** The output of our create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UpozorneniSkupiny` that was created by this mutation. */
	upozorneniSkupiny?:ValueTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ValueTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ValueTypes["Skupiny"],
upozorneniSkupinyEdge?: [{	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][]},ValueTypes["UpozorneniSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `UpozorneniSkupiny` to be created by this mutation. */
	upozorneniSkupiny:ValueTypes["UpozorneniSkupinyInput"]
};
	/** An input for mutations affecting `UpozorneniSkupiny` */
["UpozorneniSkupinyInput"]: {
	upsId?:ValueTypes["BigInt"] | null,
	upsIdRodic:ValueTypes["BigInt"],
	upsIdSkupina:ValueTypes["BigInt"],
	upsColor:string,
	upsPopis:string
};
	/** The output of our create `User` mutation. */
["CreateUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `User` that was created by this mutation. */
	user?:ValueTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ValueTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ValueTypes["Skupiny"],
userEdge?: [{	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][]},ValueTypes["UsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `User` mutation. */
["CreateUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `User` to be created by this mutation. */
	user:ValueTypes["UserInput"]
};
	/** An input for mutations affecting `User` */
["UserInput"]: {
	uId?:ValueTypes["BigInt"] | null,
	uLogin:string,
	uPass:string,
	uJmeno:string,
	uPrijmeni:string,
	uPohlavi:string,
	uEmail:string,
	uTelefon:string,
	uNarozeni:ValueTypes["Date"],
	uRodneCislo?:string | null,
	uPoznamky?:string | null,
	uTimestamp?:ValueTypes["Datetime"] | null,
	uLevel?:number | null,
	uGroup:ValueTypes["BigInt"],
	uSkupina?:ValueTypes["BigInt"] | null,
	uDancer?:boolean | null,
	uBan?:boolean | null,
	uLock?:boolean | null,
	uConfirmed?:boolean | null,
	uSystem?:boolean | null,
	uStreet:string,
	uConscriptionNumber?:string | null,
	uOrientationNumber?:string | null,
	uDistrict?:string | null,
	uCity:string,
	uPostalCode:string,
	uNationality:string,
	uMemberSince?:ValueTypes["Datetime"] | null,
	uMemberUntil?:ValueTypes["Datetime"] | null,
	uCreatedAt?:ValueTypes["Datetime"] | null,
	uTeacher?:boolean | null,
	uGdprSignedAt?:ValueTypes["Datetime"] | null
};
	/** The output of our create `UsersSkupiny` mutation. */
["CreateUsersSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UsersSkupiny` that was created by this mutation. */
	usersSkupiny?:ValueTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
usersSkupinyEdge?: [{	/** The method to use when ordering `UsersSkupiny`. */
	orderBy?:ValueTypes["UsersSkupiniesOrderBy"][]},ValueTypes["UsersSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `UsersSkupiny` mutation. */
["CreateUsersSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `UsersSkupiny` to be created by this mutation. */
	usersSkupiny:ValueTypes["UsersSkupinyInput"]
};
	/** An input for mutations affecting `UsersSkupiny` */
["UsersSkupinyInput"]: {
	usId?:ValueTypes["BigInt"] | null,
	usColor?:string | null,
	usPlatbaMesic?:ValueTypes["BigInt"] | null,
	usPlatbaCtvrtrok?:ValueTypes["BigInt"] | null,
	usPlatbaPulrok?:ValueTypes["BigInt"] | null,
	usPopis:string
};
	/** The output of our create `Video` mutation. */
["CreateVideoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Video` that was created by this mutation. */
	video?:ValueTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoEdge?: [{	/** The method to use when ordering `Video`. */
	orderBy?:ValueTypes["VideosOrderBy"][]},ValueTypes["VideosEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Video` mutation. */
["CreateVideoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Video` to be created by this mutation. */
	video:ValueTypes["VideoInput"]
};
	/** An input for mutations affecting `Video` */
["VideoInput"]: {
	vId?:ValueTypes["BigInt"] | null,
	vUri:string,
	vTitle:string,
	vAuthor:string,
	vDescription:string,
	vPlaylist?:string | null,
	vCreatedAt:ValueTypes["Datetime"],
	vUpdatedAt?:ValueTypes["Datetime"] | null
};
	/** The output of our create `VideoList` mutation. */
["CreateVideoListPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoList` that was created by this mutation. */
	videoList?:ValueTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoListEdge?: [{	/** The method to use when ordering `VideoList`. */
	orderBy?:ValueTypes["VideoListsOrderBy"][]},ValueTypes["VideoListsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `VideoList` mutation. */
["CreateVideoListInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `VideoList` to be created by this mutation. */
	videoList:ValueTypes["VideoListInput"]
};
	/** An input for mutations affecting `VideoList` */
["VideoListInput"]: {
	vlId?:ValueTypes["BigInt"] | null,
	vlUrl:string,
	vlTitle:string,
	vlDescription:string,
	vlCount:ValueTypes["BigInt"],
	vlCreatedAt:ValueTypes["Datetime"],
	vlLastChecked?:ValueTypes["Datetime"] | null
};
	/** The output of our create `VideoSource` mutation. */
["CreateVideoSourcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoSource` that was created by this mutation. */
	videoSource?:ValueTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoSourceEdge?: [{	/** The method to use when ordering `VideoSource`. */
	orderBy?:ValueTypes["VideoSourcesOrderBy"][]},ValueTypes["VideoSourcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `VideoSource` mutation. */
["CreateVideoSourceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `VideoSource` to be created by this mutation. */
	videoSource:ValueTypes["VideoSourceInput"]
};
	/** An input for mutations affecting `VideoSource` */
["VideoSourceInput"]: {
	vsId?:ValueTypes["BigInt"] | null,
	vsUrl:string,
	vsTitle?:string | null,
	vsDescription?:string | null,
	vsCreatedAt?:ValueTypes["Datetime"] | null,
	vsLastChecked?:ValueTypes["Datetime"] | null
};
	/** The output of our update `Akce` mutation. */
["UpdateAkcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Akce` that was updated by this mutation. */
	akce?:ValueTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
akceEdge?: [{	/** The method to use when ordering `Akce`. */
	orderBy?:ValueTypes["AkcesOrderBy"][]},ValueTypes["AkcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAkceByNodeId` mutation. */
["UpdateAkceByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Akce` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Akce` being updated. */
	patch:ValueTypes["AkcePatch"]
};
	/** Represents an update to a `Akce`. Fields that are set will be updated. */
["AkcePatch"]: {
	aId?:ValueTypes["BigInt"] | null,
	aJmeno?:string | null,
	aKde?:string | null,
	aInfo?:string | null,
	aOd?:ValueTypes["Date"] | null,
	aDo?:ValueTypes["Date"] | null,
	aKapacita?:ValueTypes["BigInt"] | null,
	aDokumenty?:string | null,
	aTimestamp?:ValueTypes["Datetime"] | null,
	aLock?:boolean | null,
	aVisible?:boolean | null
};
	/** All input for the `updateAkce` mutation. */
["UpdateAkceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Akce` being updated. */
	patch:ValueTypes["AkcePatch"],
	aId:ValueTypes["BigInt"]
};
	/** The output of our update `AkceItem` mutation. */
["UpdateAkceItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AkceItem` that was updated by this mutation. */
	akceItem?:ValueTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ValueTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ValueTypes["User"],
akceItemEdge?: [{	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAkceItemByNodeId` mutation. */
["UpdateAkceItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `AkceItem` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `AkceItem` being updated. */
	patch:ValueTypes["AkceItemPatch"]
};
	/** Represents an update to a `AkceItem`. Fields that are set will be updated. */
["AkceItemPatch"]: {
	aiId?:ValueTypes["BigInt"] | null,
	aiIdRodic?:ValueTypes["BigInt"] | null,
	aiUser?:ValueTypes["BigInt"] | null,
	aiRokNarozeni?:number | null
};
	/** All input for the `updateAkceItem` mutation. */
["UpdateAkceItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `AkceItem` being updated. */
	patch:ValueTypes["AkceItemPatch"],
	aiId:ValueTypes["BigInt"]
};
	/** The output of our update `Aktuality` mutation. */
["UpdateAktualityPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Aktuality` that was updated by this mutation. */
	aktuality?:ValueTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ValueTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ValueTypes["GalerieFoto"],
aktualityEdge?: [{	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][]},ValueTypes["AktualitiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAktualityByNodeId` mutation. */
["UpdateAktualityByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Aktuality` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch:ValueTypes["AktualityPatch"]
};
	/** Represents an update to a `Aktuality`. Fields that are set will be updated. */
["AktualityPatch"]: {
	atId?:ValueTypes["BigInt"] | null,
	atKdo?:ValueTypes["BigInt"] | null,
	atKat?:string | null,
	atJmeno?:string | null,
	atText?:string | null,
	atPreview?:string | null,
	atFoto?:ValueTypes["BigInt"] | null,
	atFotoMain?:ValueTypes["BigInt"] | null,
	atTimestamp?:ValueTypes["Datetime"] | null,
	atTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch:ValueTypes["AktualityPatch"],
	atId:ValueTypes["BigInt"]
};
	/** The output of our update `Dokumenty` mutation. */
["UpdateDokumentyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Dokumenty` that was updated by this mutation. */
	dokumenty?:ValueTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ValueTypes["User"],
dokumentyEdge?: [{	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][]},ValueTypes["DokumentiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateDokumentyByNodeId` mutation. */
["UpdateDokumentyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Dokumenty` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch:ValueTypes["DokumentyPatch"]
};
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: {
	dId?:ValueTypes["BigInt"] | null,
	dPath?:string | null,
	dName?:string | null,
	dFilename?:string | null,
	dKategorie?:number | null,
	dKdo?:ValueTypes["BigInt"] | null,
	dTimestamp?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch:ValueTypes["DokumentyPatch"],
	dId:ValueTypes["BigInt"]
};
	/** The output of our update `GalerieDir` mutation. */
["UpdateGalerieDirPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieDir` that was updated by this mutation. */
	galerieDir?:ValueTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
galerieDirEdge?: [{	/** The method to use when ordering `GalerieDir`. */
	orderBy?:ValueTypes["GalerieDirsOrderBy"][]},ValueTypes["GalerieDirsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateGalerieDirByNodeId` mutation. */
["UpdateGalerieDirByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `GalerieDir` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch:ValueTypes["GalerieDirPatch"]
};
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: {
	gdId?:ValueTypes["BigInt"] | null,
	gdIdRodic?:ValueTypes["BigInt"] | null,
	gdName?:string | null,
	gdLevel?:number | null,
	gdPath?:string | null,
	gdHidden?:boolean | null
};
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch:ValueTypes["GalerieDirPatch"],
	gdId:ValueTypes["BigInt"]
};
	/** The output of our update `GalerieFoto` mutation. */
["UpdateGalerieFotoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieFoto` that was updated by this mutation. */
	galerieFoto?:ValueTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ValueTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ValueTypes["User"],
galerieFotoEdge?: [{	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][]},ValueTypes["GalerieFotosEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateGalerieFotoByNodeId` mutation. */
["UpdateGalerieFotoByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `GalerieFoto` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch:ValueTypes["GalerieFotoPatch"]
};
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: {
	gfId?:ValueTypes["BigInt"] | null,
	gfIdRodic?:ValueTypes["BigInt"] | null,
	gfName?:string | null,
	gfPath?:string | null,
	gfKdo?:ValueTypes["BigInt"] | null,
	gfTimestamp?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch:ValueTypes["GalerieFotoPatch"],
	gfId:ValueTypes["BigInt"]
};
	/** The output of our update `Nabidka` mutation. */
["UpdateNabidkaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Nabidka` that was updated by this mutation. */
	nabidka?:ValueTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ValueTypes["User"],
nabidkaEdge?: [{	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][]},ValueTypes["NabidkasEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateNabidkaByNodeId` mutation. */
["UpdateNabidkaByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Nabidka` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch:ValueTypes["NabidkaPatch"]
};
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: {
	nId?:ValueTypes["BigInt"] | null,
	nTrener?:ValueTypes["BigInt"] | null,
	nPocetHod?:number | null,
	nMaxPocetHod?:ValueTypes["BigInt"] | null,
	nOd?:ValueTypes["Date"] | null,
	nDo?:ValueTypes["Date"] | null,
	nVisible?:boolean | null,
	nLock?:boolean | null,
	nTimestamp?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch:ValueTypes["NabidkaPatch"],
	nId:ValueTypes["BigInt"]
};
	/** The output of our update `NabidkaItem` mutation. */
["UpdateNabidkaItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `NabidkaItem` that was updated by this mutation. */
	nabidkaItem?:ValueTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ValueTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ValueTypes["Pary"],
nabidkaItemEdge?: [{	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][]},ValueTypes["NabidkaItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateNabidkaItemByNodeId` mutation. */
["UpdateNabidkaItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `NabidkaItem` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch:ValueTypes["NabidkaItemPatch"]
};
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: {
	niId?:ValueTypes["BigInt"] | null,
	niIdRodic?:ValueTypes["BigInt"] | null,
	niPartner?:ValueTypes["BigInt"] | null,
	niPocetHod?:number | null,
	niLock?:boolean | null
};
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch:ValueTypes["NabidkaItemPatch"],
	niId:ValueTypes["BigInt"]
};
	/** The output of our update `Page` mutation. */
["UpdatePagePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Page` that was updated by this mutation. */
	page?:ValueTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
pageEdge?: [{	/** The method to use when ordering `Page`. */
	orderBy?:ValueTypes["PagesOrderBy"][]},ValueTypes["PagesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePageByNodeId` mutation. */
["UpdatePageByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Page` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch:ValueTypes["PagePatch"]
};
	/** Represents an update to a `Page`. Fields that are set will be updated. */
["PagePatch"]: {
	id?:number | null,
	url?:string | null,
	content?:ValueTypes["JSON"] | null,
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	title?:string | null
};
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch:ValueTypes["PagePatch"],
	id:number
};
	/** All input for the `updatePageByUrl` mutation. */
["UpdatePageByUrlInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch:ValueTypes["PagePatch"],
	url:string
};
	/** The output of our update `Parameter` mutation. */
["UpdateParameterPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Parameter` that was updated by this mutation. */
	parameter?:ValueTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
parameterEdge?: [{	/** The method to use when ordering `Parameter`. */
	orderBy?:ValueTypes["ParametersOrderBy"][]},ValueTypes["ParametersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateParameterByNodeId` mutation. */
["UpdateParameterByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Parameter` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch:ValueTypes["ParameterPatch"]
};
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: {
	paName?:string | null,
	paValue?:string | null
};
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch:ValueTypes["ParameterPatch"],
	paName:string
};
	/** The output of our update `Pary` mutation. */
["UpdateParyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Pary` that was updated by this mutation. */
	pary?:ValueTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ValueTypes["User"],
paryEdge?: [{	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][]},ValueTypes["PariesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateParyByNodeId` mutation. */
["UpdateParyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Pary` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Pary` being updated. */
	patch:ValueTypes["ParyPatch"]
};
	/** Represents an update to a `Pary`. Fields that are set will be updated. */
["ParyPatch"]: {
	pId?:ValueTypes["BigInt"] | null,
	pIdPartner?:ValueTypes["BigInt"] | null,
	pIdPartnerka?:ValueTypes["BigInt"] | null,
	pSttTrida?:ValueTypes["ParyPSttTrida"] | null,
	pSttBody?:number | null,
	pSttFinale?:boolean | null,
	pLatTrida?:ValueTypes["ParyPLatTrida"] | null,
	pLatBody?:number | null,
	pLatFinale?:boolean | null,
	pHodnoceni?:number | null,
	pArchiv?:boolean | null,
	pTimestampAdd?:ValueTypes["Datetime"] | null,
	pTimestampArchive?:ValueTypes["Datetime"] | null
};
	/** All input for the `updatePary` mutation. */
["UpdateParyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Pary` being updated. */
	patch:ValueTypes["ParyPatch"],
	pId:ValueTypes["BigInt"]
};
	/** The output of our update `ParyNavrh` mutation. */
["UpdateParyNavrhPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `ParyNavrh` that was updated by this mutation. */
	paryNavrh?:ValueTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ValueTypes["User"],
paryNavrhEdge?: [{	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][]},ValueTypes["ParyNavrhsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateParyNavrhByNodeId` mutation. */
["UpdateParyNavrhByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `ParyNavrh` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch:ValueTypes["ParyNavrhPatch"]
};
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: {
	pnId?:ValueTypes["BigInt"] | null,
	pnNavrhl?:ValueTypes["BigInt"] | null,
	pnPartner?:ValueTypes["BigInt"] | null,
	pnPartnerka?:ValueTypes["BigInt"] | null
};
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch:ValueTypes["ParyNavrhPatch"],
	pnId:ValueTypes["BigInt"]
};
	/** The output of our update `Permission` mutation. */
["UpdatePermissionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Permission` that was updated by this mutation. */
	permission?:ValueTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
permissionEdge?: [{	/** The method to use when ordering `Permission`. */
	orderBy?:ValueTypes["PermissionsOrderBy"][]},ValueTypes["PermissionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePermissionByNodeId` mutation. */
["UpdatePermissionByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Permission` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch:ValueTypes["PermissionPatch"]
};
	/** Represents an update to a `Permission`. Fields that are set will be updated. */
["PermissionPatch"]: {
	peId?:ValueTypes["BigInt"] | null,
	peName?:string | null,
	peDescription?:string | null,
	peAkce?:number | null,
	peAktuality?:number | null,
	peAnkety?:number | null,
	peDokumenty?:number | null,
	peGalerie?:number | null,
	peInzerce?:number | null,
	peKonzole?:number | null,
	peNabidka?:number | null,
	peNastenka?:number | null,
	peNovinky?:number | null,
	pePary?:number | null,
	pePlatby?:number | null,
	pePermissions?:number | null,
	peRozpis?:number | null,
	peSkupiny?:number | null,
	peUsers?:number | null,
	peMain?:number | null
};
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch:ValueTypes["PermissionPatch"],
	peId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyCategory` mutation. */
["UpdatePlatbyCategoryPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategory` that was updated by this mutation. */
	platbyCategory?:ValueTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyCategoryEdge?: [{	/** The method to use when ordering `PlatbyCategory`. */
	orderBy?:ValueTypes["PlatbyCategoriesOrderBy"][]},ValueTypes["PlatbyCategoriesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyCategoryByNodeId` mutation. */
["UpdatePlatbyCategoryByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyCategory` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch:ValueTypes["PlatbyCategoryPatch"]
};
	/** Represents an update to a `PlatbyCategory`. Fields that are set will be updated. */
["PlatbyCategoryPatch"]: {
	pcId?:ValueTypes["BigInt"] | null,
	pcName?:string | null,
	pcSymbol?:ValueTypes["BigInt"] | null,
	pcAmount?:ValueTypes["BigFloat"] | null,
	pcDateDue?:ValueTypes["Date"] | null,
	pcValidFrom?:ValueTypes["Date"] | null,
	pcValidTo?:ValueTypes["Date"] | null,
	pcUseBase?:boolean | null,
	pcUsePrefix?:boolean | null,
	pcArchive?:boolean | null,
	pcVisible?:boolean | null
};
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch:ValueTypes["PlatbyCategoryPatch"],
	pcId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategoryGroup` that was updated by this mutation. */
	platbyCategoryGroup?:ValueTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ValueTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ValueTypes["PlatbyCategory"],
platbyCategoryGroupEdge?: [{	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][]},ValueTypes["PlatbyCategoryGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyCategoryGroupByNodeId` mutation. */
["UpdatePlatbyCategoryGroupByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch:ValueTypes["PlatbyCategoryGroupPatch"]
};
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: {
	pcgId?:ValueTypes["BigInt"] | null,
	pcgIdGroup?:ValueTypes["BigInt"] | null,
	pcgIdCategory?:ValueTypes["BigInt"] | null
};
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch:ValueTypes["PlatbyCategoryGroupPatch"],
	pcgId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyGroup` mutation. */
["UpdatePlatbyGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroup` that was updated by this mutation. */
	platbyGroup?:ValueTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyGroupEdge?: [{	/** The method to use when ordering `PlatbyGroup`. */
	orderBy?:ValueTypes["PlatbyGroupsOrderBy"][]},ValueTypes["PlatbyGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyGroupByNodeId` mutation. */
["UpdatePlatbyGroupByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyGroup` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch:ValueTypes["PlatbyGroupPatch"]
};
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: {
	pgId?:ValueTypes["BigInt"] | null,
	pgType?:ValueTypes["BigFloat"] | null,
	pgName?:string | null,
	pgDescription?:string | null,
	pgBase?:ValueTypes["BigInt"] | null
};
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch:ValueTypes["PlatbyGroupPatch"],
	pgId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroupSkupina` that was updated by this mutation. */
	platbyGroupSkupina?:ValueTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ValueTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ValueTypes["PlatbyGroup"],
platbyGroupSkupinaEdge?: [{	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][]},ValueTypes["PlatbyGroupSkupinasEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyGroupSkupinaByNodeId` mutation. */
["UpdatePlatbyGroupSkupinaByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch:ValueTypes["PlatbyGroupSkupinaPatch"]
};
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: {
	pgsId?:ValueTypes["BigInt"] | null,
	pgsIdSkupina?:ValueTypes["BigInt"] | null,
	pgsIdGroup?:ValueTypes["BigInt"] | null
};
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch:ValueTypes["PlatbyGroupSkupinaPatch"],
	pgsId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyItem` mutation. */
["UpdatePlatbyItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyItem` that was updated by this mutation. */
	platbyItem?:ValueTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ValueTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ValueTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ValueTypes["PlatbyRaw"],
platbyItemEdge?: [{	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][]},ValueTypes["PlatbyItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyItemByNodeId` mutation. */
["UpdatePlatbyItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyItem` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch:ValueTypes["PlatbyItemPatch"]
};
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: {
	piId?:ValueTypes["BigInt"] | null,
	piIdUser?:ValueTypes["BigInt"] | null,
	piIdCategory?:ValueTypes["BigInt"] | null,
	piIdRaw?:ValueTypes["BigInt"] | null,
	piAmount?:ValueTypes["BigFloat"] | null,
	piDate?:ValueTypes["Date"] | null,
	piPrefix?:number | null
};
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch:ValueTypes["PlatbyItemPatch"],
	piId:ValueTypes["BigInt"]
};
	/** The output of our update `PlatbyRaw` mutation. */
["UpdatePlatbyRawPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyRaw` that was updated by this mutation. */
	platbyRaw?:ValueTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyRawEdge?: [{	/** The method to use when ordering `PlatbyRaw`. */
	orderBy?:ValueTypes["PlatbyRawsOrderBy"][]},ValueTypes["PlatbyRawsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePlatbyRawByNodeId` mutation. */
["UpdatePlatbyRawByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyRaw` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch:ValueTypes["PlatbyRawPatch"]
};
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: {
	prId?:ValueTypes["BigInt"] | null,
	prRaw?:string | null,
	prHash?:string | null,
	prSorted?:boolean | null,
	prDiscarded?:boolean | null
};
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch:ValueTypes["PlatbyRawPatch"],
	prId:ValueTypes["BigInt"]
};
	/** The output of our update `Rozpi` mutation. */
["UpdateRozpiPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Rozpi` that was updated by this mutation. */
	rozpi?:ValueTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ValueTypes["User"],
rozpiEdge?: [{	/** The method to use when ordering `Rozpi`. */
	orderBy?:ValueTypes["RozpisOrderBy"][]},ValueTypes["RozpisEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateRozpiByNodeId` mutation. */
["UpdateRozpiByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Rozpi` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch:ValueTypes["RozpiPatch"]
};
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: {
	rId?:ValueTypes["BigInt"] | null,
	rTrener?:ValueTypes["BigInt"] | null,
	rKde?:string | null,
	rDatum?:ValueTypes["Date"] | null,
	rVisible?:boolean | null,
	rLock?:boolean | null,
	rTimestamp?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch:ValueTypes["RozpiPatch"],
	rId:ValueTypes["BigInt"]
};
	/** The output of our update `RozpisItem` mutation. */
["UpdateRozpisItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RozpisItem` that was updated by this mutation. */
	rozpisItem?:ValueTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ValueTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ValueTypes["Pary"],
rozpisItemEdge?: [{	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][]},ValueTypes["RozpisItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateRozpisItemByNodeId` mutation. */
["UpdateRozpisItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `RozpisItem` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch:ValueTypes["RozpisItemPatch"]
};
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: {
	riId?:ValueTypes["BigInt"] | null,
	riIdRodic?:ValueTypes["BigInt"] | null,
	riPartner?:ValueTypes["BigInt"] | null,
	riOd?:ValueTypes["Time"] | null,
	riDo?:ValueTypes["Time"] | null,
	riLock?:boolean | null
};
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch:ValueTypes["RozpisItemPatch"],
	riId:ValueTypes["BigInt"]
};
	/** The output of our update `Session` mutation. */
["UpdateSessionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Session` that was updated by this mutation. */
	session?:ValueTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ValueTypes["User"],
sessionEdge?: [{	/** The method to use when ordering `Session`. */
	orderBy?:ValueTypes["SessionsOrderBy"][]},ValueTypes["SessionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateSessionByNodeId` mutation. */
["UpdateSessionByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Session` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Session` being updated. */
	patch:ValueTypes["SessionPatch"]
};
	/** Represents an update to a `Session`. Fields that are set will be updated. */
["SessionPatch"]: {
	ssId?:string | null,
	ssData?:string | null,
	ssUpdatedAt?:ValueTypes["Datetime"] | null,
	ssLifetime?:ValueTypes["BigInt"] | null,
	ssUser?:ValueTypes["BigInt"] | null
};
	/** All input for the `updateSession` mutation. */
["UpdateSessionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Session` being updated. */
	patch:ValueTypes["SessionPatch"],
	ssId:string
};
	/** The output of our update `Skupiny` mutation. */
["UpdateSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Skupiny` that was updated by this mutation. */
	skupiny?:ValueTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
skupinyEdge?: [{	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][]},ValueTypes["SkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateSkupinyByNodeId` mutation. */
["UpdateSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Skupiny` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Skupiny` being updated. */
	patch:ValueTypes["SkupinyPatch"]
};
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: {
	sId?:ValueTypes["BigInt"] | null,
	sName?:string | null,
	sDescription?:string | null,
	sColorRgb?:string | null,
	sColorText?:string | null,
	sLocation?:string | null,
	sVisible?:boolean | null
};
	/** All input for the `updateSkupiny` mutation. */
["UpdateSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Skupiny` being updated. */
	patch:ValueTypes["SkupinyPatch"],
	sId:ValueTypes["BigInt"]
};
	/** The output of our update `Upozorneni` mutation. */
["UpdateUpozorneniPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Upozorneni` that was updated by this mutation. */
	upozorneni?:ValueTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ValueTypes["User"],
upozorneniEdge?: [{	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][]},ValueTypes["UpozornenisEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateUpozorneniByNodeId` mutation. */
["UpdateUpozorneniByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Upozorneni` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch:ValueTypes["UpozorneniPatch"]
};
	/** Represents an update to a `Upozorneni`. Fields that are set will be updated. */
["UpozorneniPatch"]: {
	upId?:ValueTypes["BigInt"] | null,
	upKdo?:ValueTypes["BigInt"] | null,
	upNadpis?:string | null,
	upText?:string | null,
	upBarvy?:ValueTypes["BigInt"] | null,
	upLock?:boolean | null,
	upTimestamp?:ValueTypes["Datetime"] | null,
	upTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch:ValueTypes["UpozorneniPatch"],
	upId:ValueTypes["BigInt"]
};
	/** The output of our update `UpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UpozorneniSkupiny` that was updated by this mutation. */
	upozorneniSkupiny?:ValueTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ValueTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ValueTypes["Skupiny"],
upozorneniSkupinyEdge?: [{	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][]},ValueTypes["UpozorneniSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateUpozorneniSkupinyByNodeId` mutation. */
["UpdateUpozorneniSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch:ValueTypes["UpozorneniSkupinyPatch"]
};
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: {
	upsId?:ValueTypes["BigInt"] | null,
	upsIdRodic?:ValueTypes["BigInt"] | null,
	upsIdSkupina?:ValueTypes["BigInt"] | null,
	upsColor?:string | null,
	upsPopis?:string | null
};
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch:ValueTypes["UpozorneniSkupinyPatch"],
	upsId:ValueTypes["BigInt"]
};
	/** The output of our update `User` mutation. */
["UpdateUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `User` that was updated by this mutation. */
	user?:ValueTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ValueTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ValueTypes["Skupiny"],
userEdge?: [{	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][]},ValueTypes["UsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateUserByNodeId` mutation. */
["UpdateUserByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `User` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch:ValueTypes["UserPatch"]
};
	/** Represents an update to a `User`. Fields that are set will be updated. */
["UserPatch"]: {
	uId?:ValueTypes["BigInt"] | null,
	uLogin?:string | null,
	uPass?:string | null,
	uJmeno?:string | null,
	uPrijmeni?:string | null,
	uPohlavi?:string | null,
	uEmail?:string | null,
	uTelefon?:string | null,
	uNarozeni?:ValueTypes["Date"] | null,
	uRodneCislo?:string | null,
	uPoznamky?:string | null,
	uTimestamp?:ValueTypes["Datetime"] | null,
	uLevel?:number | null,
	uGroup?:ValueTypes["BigInt"] | null,
	uSkupina?:ValueTypes["BigInt"] | null,
	uDancer?:boolean | null,
	uBan?:boolean | null,
	uLock?:boolean | null,
	uConfirmed?:boolean | null,
	uSystem?:boolean | null,
	uStreet?:string | null,
	uConscriptionNumber?:string | null,
	uOrientationNumber?:string | null,
	uDistrict?:string | null,
	uCity?:string | null,
	uPostalCode?:string | null,
	uNationality?:string | null,
	uMemberSince?:ValueTypes["Datetime"] | null,
	uMemberUntil?:ValueTypes["Datetime"] | null,
	uCreatedAt?:ValueTypes["Datetime"] | null,
	uTeacher?:boolean | null,
	uGdprSignedAt?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch:ValueTypes["UserPatch"],
	uId:ValueTypes["BigInt"]
};
	/** The output of our update `UsersSkupiny` mutation. */
["UpdateUsersSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UsersSkupiny` that was updated by this mutation. */
	usersSkupiny?:ValueTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
usersSkupinyEdge?: [{	/** The method to use when ordering `UsersSkupiny`. */
	orderBy?:ValueTypes["UsersSkupiniesOrderBy"][]},ValueTypes["UsersSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateUsersSkupinyByNodeId` mutation. */
["UpdateUsersSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `UsersSkupiny` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
	patch:ValueTypes["UsersSkupinyPatch"]
};
	/** Represents an update to a `UsersSkupiny`. Fields that are set will be updated. */
["UsersSkupinyPatch"]: {
	usId?:ValueTypes["BigInt"] | null,
	usColor?:string | null,
	usPlatbaMesic?:ValueTypes["BigInt"] | null,
	usPlatbaCtvrtrok?:ValueTypes["BigInt"] | null,
	usPlatbaPulrok?:ValueTypes["BigInt"] | null,
	usPopis?:string | null
};
	/** All input for the `updateUsersSkupiny` mutation. */
["UpdateUsersSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
	patch:ValueTypes["UsersSkupinyPatch"],
	usId:ValueTypes["BigInt"]
};
	/** The output of our update `Video` mutation. */
["UpdateVideoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Video` that was updated by this mutation. */
	video?:ValueTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoEdge?: [{	/** The method to use when ordering `Video`. */
	orderBy?:ValueTypes["VideosOrderBy"][]},ValueTypes["VideosEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateVideoByNodeId` mutation. */
["UpdateVideoByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Video` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `Video` being updated. */
	patch:ValueTypes["VideoPatch"]
};
	/** Represents an update to a `Video`. Fields that are set will be updated. */
["VideoPatch"]: {
	vId?:ValueTypes["BigInt"] | null,
	vUri?:string | null,
	vTitle?:string | null,
	vAuthor?:string | null,
	vDescription?:string | null,
	vPlaylist?:string | null,
	vCreatedAt?:ValueTypes["Datetime"] | null,
	vUpdatedAt?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateVideo` mutation. */
["UpdateVideoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Video` being updated. */
	patch:ValueTypes["VideoPatch"],
	vId:ValueTypes["BigInt"]
};
	/** The output of our update `VideoList` mutation. */
["UpdateVideoListPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoList` that was updated by this mutation. */
	videoList?:ValueTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoListEdge?: [{	/** The method to use when ordering `VideoList`. */
	orderBy?:ValueTypes["VideoListsOrderBy"][]},ValueTypes["VideoListsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateVideoListByNodeId` mutation. */
["UpdateVideoListByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `VideoList` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `VideoList` being updated. */
	patch:ValueTypes["VideoListPatch"]
};
	/** Represents an update to a `VideoList`. Fields that are set will be updated. */
["VideoListPatch"]: {
	vlId?:ValueTypes["BigInt"] | null,
	vlUrl?:string | null,
	vlTitle?:string | null,
	vlDescription?:string | null,
	vlCount?:ValueTypes["BigInt"] | null,
	vlCreatedAt?:ValueTypes["Datetime"] | null,
	vlLastChecked?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateVideoList` mutation. */
["UpdateVideoListInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `VideoList` being updated. */
	patch:ValueTypes["VideoListPatch"],
	vlId:ValueTypes["BigInt"]
};
	/** The output of our update `VideoSource` mutation. */
["UpdateVideoSourcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoSource` that was updated by this mutation. */
	videoSource?:ValueTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoSourceEdge?: [{	/** The method to use when ordering `VideoSource`. */
	orderBy?:ValueTypes["VideoSourcesOrderBy"][]},ValueTypes["VideoSourcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateVideoSourceByNodeId` mutation. */
["UpdateVideoSourceByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `VideoSource` to be updated. */
	nodeId:string,
	/** An object where the defined keys will be set on the `VideoSource` being updated. */
	patch:ValueTypes["VideoSourcePatch"]
};
	/** Represents an update to a `VideoSource`. Fields that are set will be updated. */
["VideoSourcePatch"]: {
	vsId?:ValueTypes["BigInt"] | null,
	vsUrl?:string | null,
	vsTitle?:string | null,
	vsDescription?:string | null,
	vsCreatedAt?:ValueTypes["Datetime"] | null,
	vsLastChecked?:ValueTypes["Datetime"] | null
};
	/** All input for the `updateVideoSource` mutation. */
["UpdateVideoSourceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `VideoSource` being updated. */
	patch:ValueTypes["VideoSourcePatch"],
	vsId:ValueTypes["BigInt"]
};
	/** The output of our delete `Akce` mutation. */
["DeleteAkcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Akce` that was deleted by this mutation. */
	akce?:ValueTypes["Akce"],
	deletedAkceNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
akceEdge?: [{	/** The method to use when ordering `Akce`. */
	orderBy?:ValueTypes["AkcesOrderBy"][]},ValueTypes["AkcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAkceByNodeId` mutation. */
["DeleteAkceByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Akce` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteAkce` mutation. */
["DeleteAkceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	aId:ValueTypes["BigInt"]
};
	/** The output of our delete `AkceItem` mutation. */
["DeleteAkceItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AkceItem` that was deleted by this mutation. */
	akceItem?:ValueTypes["AkceItem"],
	deletedAkceItemNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ValueTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ValueTypes["User"],
akceItemEdge?: [{	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAkceItemByNodeId` mutation. */
["DeleteAkceItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `AkceItem` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteAkceItem` mutation. */
["DeleteAkceItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	aiId:ValueTypes["BigInt"]
};
	/** The output of our delete `Aktuality` mutation. */
["DeleteAktualityPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Aktuality` that was deleted by this mutation. */
	aktuality?:ValueTypes["Aktuality"],
	deletedAktualityNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ValueTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ValueTypes["GalerieFoto"],
aktualityEdge?: [{	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][]},ValueTypes["AktualitiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAktualityByNodeId` mutation. */
["DeleteAktualityByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Aktuality` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	atId:ValueTypes["BigInt"]
};
	/** The output of our delete `Dokumenty` mutation. */
["DeleteDokumentyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Dokumenty` that was deleted by this mutation. */
	dokumenty?:ValueTypes["Dokumenty"],
	deletedDokumentyNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ValueTypes["User"],
dokumentyEdge?: [{	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][]},ValueTypes["DokumentiesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteDokumentyByNodeId` mutation. */
["DeleteDokumentyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Dokumenty` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	dId:ValueTypes["BigInt"]
};
	/** The output of our delete `GalerieDir` mutation. */
["DeleteGalerieDirPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieDir` that was deleted by this mutation. */
	galerieDir?:ValueTypes["GalerieDir"],
	deletedGalerieDirNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
galerieDirEdge?: [{	/** The method to use when ordering `GalerieDir`. */
	orderBy?:ValueTypes["GalerieDirsOrderBy"][]},ValueTypes["GalerieDirsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteGalerieDirByNodeId` mutation. */
["DeleteGalerieDirByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `GalerieDir` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteGalerieDir` mutation. */
["DeleteGalerieDirInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	gdId:ValueTypes["BigInt"]
};
	/** The output of our delete `GalerieFoto` mutation. */
["DeleteGalerieFotoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `GalerieFoto` that was deleted by this mutation. */
	galerieFoto?:ValueTypes["GalerieFoto"],
	deletedGalerieFotoNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ValueTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ValueTypes["User"],
galerieFotoEdge?: [{	/** The method to use when ordering `GalerieFoto`. */
	orderBy?:ValueTypes["GalerieFotosOrderBy"][]},ValueTypes["GalerieFotosEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteGalerieFotoByNodeId` mutation. */
["DeleteGalerieFotoByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `GalerieFoto` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	gfId:ValueTypes["BigInt"]
};
	/** The output of our delete `Nabidka` mutation. */
["DeleteNabidkaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Nabidka` that was deleted by this mutation. */
	nabidka?:ValueTypes["Nabidka"],
	deletedNabidkaNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ValueTypes["User"],
nabidkaEdge?: [{	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][]},ValueTypes["NabidkasEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteNabidkaByNodeId` mutation. */
["DeleteNabidkaByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Nabidka` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteNabidka` mutation. */
["DeleteNabidkaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	nId:ValueTypes["BigInt"]
};
	/** The output of our delete `NabidkaItem` mutation. */
["DeleteNabidkaItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `NabidkaItem` that was deleted by this mutation. */
	nabidkaItem?:ValueTypes["NabidkaItem"],
	deletedNabidkaItemNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ValueTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ValueTypes["Pary"],
nabidkaItemEdge?: [{	/** The method to use when ordering `NabidkaItem`. */
	orderBy?:ValueTypes["NabidkaItemsOrderBy"][]},ValueTypes["NabidkaItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteNabidkaItemByNodeId` mutation. */
["DeleteNabidkaItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `NabidkaItem` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	niId:ValueTypes["BigInt"]
};
	/** The output of our delete `Parameter` mutation. */
["DeleteParameterPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Parameter` that was deleted by this mutation. */
	parameter?:ValueTypes["Parameter"],
	deletedParameterNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
parameterEdge?: [{	/** The method to use when ordering `Parameter`. */
	orderBy?:ValueTypes["ParametersOrderBy"][]},ValueTypes["ParametersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteParameterByNodeId` mutation. */
["DeleteParameterByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Parameter` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteParameter` mutation. */
["DeleteParameterInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	paName:string
};
	/** The output of our delete `Pary` mutation. */
["DeleteParyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Pary` that was deleted by this mutation. */
	pary?:ValueTypes["Pary"],
	deletedParyNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ValueTypes["User"],
paryEdge?: [{	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][]},ValueTypes["PariesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteParyByNodeId` mutation. */
["DeleteParyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Pary` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePary` mutation. */
["DeleteParyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pId:ValueTypes["BigInt"]
};
	/** The output of our delete `ParyNavrh` mutation. */
["DeleteParyNavrhPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `ParyNavrh` that was deleted by this mutation. */
	paryNavrh?:ValueTypes["ParyNavrh"],
	deletedParyNavrhNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ValueTypes["User"],
paryNavrhEdge?: [{	/** The method to use when ordering `ParyNavrh`. */
	orderBy?:ValueTypes["ParyNavrhsOrderBy"][]},ValueTypes["ParyNavrhsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteParyNavrhByNodeId` mutation. */
["DeleteParyNavrhByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `ParyNavrh` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteParyNavrh` mutation. */
["DeleteParyNavrhInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pnId:ValueTypes["BigInt"]
};
	/** The output of our delete `Permission` mutation. */
["DeletePermissionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Permission` that was deleted by this mutation. */
	permission?:ValueTypes["Permission"],
	deletedPermissionNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
permissionEdge?: [{	/** The method to use when ordering `Permission`. */
	orderBy?:ValueTypes["PermissionsOrderBy"][]},ValueTypes["PermissionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePermissionByNodeId` mutation. */
["DeletePermissionByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Permission` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	peId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyCategory` mutation. */
["DeletePlatbyCategoryPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategory` that was deleted by this mutation. */
	platbyCategory?:ValueTypes["PlatbyCategory"],
	deletedPlatbyCategoryNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyCategoryEdge?: [{	/** The method to use when ordering `PlatbyCategory`. */
	orderBy?:ValueTypes["PlatbyCategoriesOrderBy"][]},ValueTypes["PlatbyCategoriesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyCategoryByNodeId` mutation. */
["DeletePlatbyCategoryByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyCategory` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyCategory` mutation. */
["DeletePlatbyCategoryInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pcId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyCategoryGroup` that was deleted by this mutation. */
	platbyCategoryGroup?:ValueTypes["PlatbyCategoryGroup"],
	deletedPlatbyCategoryGroupNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ValueTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ValueTypes["PlatbyCategory"],
platbyCategoryGroupEdge?: [{	/** The method to use when ordering `PlatbyCategoryGroup`. */
	orderBy?:ValueTypes["PlatbyCategoryGroupsOrderBy"][]},ValueTypes["PlatbyCategoryGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyCategoryGroupByNodeId` mutation. */
["DeletePlatbyCategoryGroupByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pcgId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyGroup` mutation. */
["DeletePlatbyGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroup` that was deleted by this mutation. */
	platbyGroup?:ValueTypes["PlatbyGroup"],
	deletedPlatbyGroupNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyGroupEdge?: [{	/** The method to use when ordering `PlatbyGroup`. */
	orderBy?:ValueTypes["PlatbyGroupsOrderBy"][]},ValueTypes["PlatbyGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyGroupByNodeId` mutation. */
["DeletePlatbyGroupByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyGroup` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyGroup` mutation. */
["DeletePlatbyGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pgId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyGroupSkupina` that was deleted by this mutation. */
	platbyGroupSkupina?:ValueTypes["PlatbyGroupSkupina"],
	deletedPlatbyGroupSkupinaNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ValueTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ValueTypes["PlatbyGroup"],
platbyGroupSkupinaEdge?: [{	/** The method to use when ordering `PlatbyGroupSkupina`. */
	orderBy?:ValueTypes["PlatbyGroupSkupinasOrderBy"][]},ValueTypes["PlatbyGroupSkupinasEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyGroupSkupinaByNodeId` mutation. */
["DeletePlatbyGroupSkupinaByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	pgsId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyItem` mutation. */
["DeletePlatbyItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyItem` that was deleted by this mutation. */
	platbyItem?:ValueTypes["PlatbyItem"],
	deletedPlatbyItemNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ValueTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ValueTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ValueTypes["PlatbyRaw"],
platbyItemEdge?: [{	/** The method to use when ordering `PlatbyItem`. */
	orderBy?:ValueTypes["PlatbyItemsOrderBy"][]},ValueTypes["PlatbyItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyItemByNodeId` mutation. */
["DeletePlatbyItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyItem` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyItem` mutation. */
["DeletePlatbyItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	piId:ValueTypes["BigInt"]
};
	/** The output of our delete `PlatbyRaw` mutation. */
["DeletePlatbyRawPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `PlatbyRaw` that was deleted by this mutation. */
	platbyRaw?:ValueTypes["PlatbyRaw"],
	deletedPlatbyRawNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
platbyRawEdge?: [{	/** The method to use when ordering `PlatbyRaw`. */
	orderBy?:ValueTypes["PlatbyRawsOrderBy"][]},ValueTypes["PlatbyRawsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePlatbyRawByNodeId` mutation. */
["DeletePlatbyRawByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `PlatbyRaw` to be deleted. */
	nodeId:string
};
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	prId:ValueTypes["BigInt"]
};
	/** The output of our delete `Rozpi` mutation. */
["DeleteRozpiPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Rozpi` that was deleted by this mutation. */
	rozpi?:ValueTypes["Rozpi"],
	deletedRozpiNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ValueTypes["User"],
rozpiEdge?: [{	/** The method to use when ordering `Rozpi`. */
	orderBy?:ValueTypes["RozpisOrderBy"][]},ValueTypes["RozpisEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteRozpiByNodeId` mutation. */
["DeleteRozpiByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Rozpi` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteRozpi` mutation. */
["DeleteRozpiInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	rId:ValueTypes["BigInt"]
};
	/** The output of our delete `RozpisItem` mutation. */
["DeleteRozpisItemPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RozpisItem` that was deleted by this mutation. */
	rozpisItem?:ValueTypes["RozpisItem"],
	deletedRozpisItemNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ValueTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ValueTypes["Pary"],
rozpisItemEdge?: [{	/** The method to use when ordering `RozpisItem`. */
	orderBy?:ValueTypes["RozpisItemsOrderBy"][]},ValueTypes["RozpisItemsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteRozpisItemByNodeId` mutation. */
["DeleteRozpisItemByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `RozpisItem` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	riId:ValueTypes["BigInt"]
};
	/** The output of our delete `Session` mutation. */
["DeleteSessionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Session` that was deleted by this mutation. */
	session?:ValueTypes["Session"],
	deletedSessionNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ValueTypes["User"],
sessionEdge?: [{	/** The method to use when ordering `Session`. */
	orderBy?:ValueTypes["SessionsOrderBy"][]},ValueTypes["SessionsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteSessionByNodeId` mutation. */
["DeleteSessionByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Session` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteSession` mutation. */
["DeleteSessionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	ssId:string
};
	/** The output of our delete `Skupiny` mutation. */
["DeleteSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Skupiny` that was deleted by this mutation. */
	skupiny?:ValueTypes["Skupiny"],
	deletedSkupinyNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
skupinyEdge?: [{	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][]},ValueTypes["SkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteSkupinyByNodeId` mutation. */
["DeleteSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Skupiny` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	sId:ValueTypes["BigInt"]
};
	/** The output of our delete `Upozorneni` mutation. */
["DeleteUpozorneniPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Upozorneni` that was deleted by this mutation. */
	upozorneni?:ValueTypes["Upozorneni"],
	deletedUpozorneniNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ValueTypes["User"],
upozorneniEdge?: [{	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][]},ValueTypes["UpozornenisEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteUpozorneniByNodeId` mutation. */
["DeleteUpozorneniByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Upozorneni` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteUpozorneni` mutation. */
["DeleteUpozorneniInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	upId:ValueTypes["BigInt"]
};
	/** The output of our delete `UpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UpozorneniSkupiny` that was deleted by this mutation. */
	upozorneniSkupiny?:ValueTypes["UpozorneniSkupiny"],
	deletedUpozorneniSkupinyNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ValueTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ValueTypes["Skupiny"],
upozorneniSkupinyEdge?: [{	/** The method to use when ordering `UpozorneniSkupiny`. */
	orderBy?:ValueTypes["UpozorneniSkupiniesOrderBy"][]},ValueTypes["UpozorneniSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteUpozorneniSkupinyByNodeId` mutation. */
["DeleteUpozorneniSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteUpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	upsId:ValueTypes["BigInt"]
};
	/** The output of our delete `User` mutation. */
["DeleteUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `User` that was deleted by this mutation. */
	user?:ValueTypes["User"],
	deletedUserNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ValueTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ValueTypes["Skupiny"],
userEdge?: [{	/** The method to use when ordering `User`. */
	orderBy?:ValueTypes["UsersOrderBy"][]},ValueTypes["UsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteUserByNodeId` mutation. */
["DeleteUserByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `User` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	uId:ValueTypes["BigInt"]
};
	/** The output of our delete `UsersSkupiny` mutation. */
["DeleteUsersSkupinyPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `UsersSkupiny` that was deleted by this mutation. */
	usersSkupiny?:ValueTypes["UsersSkupiny"],
	deletedUsersSkupinyNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
usersSkupinyEdge?: [{	/** The method to use when ordering `UsersSkupiny`. */
	orderBy?:ValueTypes["UsersSkupiniesOrderBy"][]},ValueTypes["UsersSkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteUsersSkupinyByNodeId` mutation. */
["DeleteUsersSkupinyByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `UsersSkupiny` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteUsersSkupiny` mutation. */
["DeleteUsersSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	usId:ValueTypes["BigInt"]
};
	/** The output of our delete `Video` mutation. */
["DeleteVideoPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Video` that was deleted by this mutation. */
	video?:ValueTypes["Video"],
	deletedVideoNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoEdge?: [{	/** The method to use when ordering `Video`. */
	orderBy?:ValueTypes["VideosOrderBy"][]},ValueTypes["VideosEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteVideoByNodeId` mutation. */
["DeleteVideoByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `Video` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteVideo` mutation. */
["DeleteVideoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	vId:ValueTypes["BigInt"]
};
	/** The output of our delete `VideoList` mutation. */
["DeleteVideoListPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoList` that was deleted by this mutation. */
	videoList?:ValueTypes["VideoList"],
	deletedVideoListNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoListEdge?: [{	/** The method to use when ordering `VideoList`. */
	orderBy?:ValueTypes["VideoListsOrderBy"][]},ValueTypes["VideoListsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteVideoListByNodeId` mutation. */
["DeleteVideoListByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `VideoList` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteVideoList` mutation. */
["DeleteVideoListInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	vlId:ValueTypes["BigInt"]
};
	/** The output of our delete `VideoSource` mutation. */
["DeleteVideoSourcePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `VideoSource` that was deleted by this mutation. */
	videoSource?:ValueTypes["VideoSource"],
	deletedVideoSourceNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
videoSourceEdge?: [{	/** The method to use when ordering `VideoSource`. */
	orderBy?:ValueTypes["VideoSourcesOrderBy"][]},ValueTypes["VideoSourcesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteVideoSourceByNodeId` mutation. */
["DeleteVideoSourceByNodeIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The globally unique `ID` which will identify a single `VideoSource` to be deleted. */
	nodeId:string
};
	/** All input for the `deleteVideoSource` mutation. */
["DeleteVideoSourceInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	vsId:ValueTypes["BigInt"]
};
	/** The output of our `login` mutation. */
["LoginPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	result?:ValueTypes["LoginRecord"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** The return type of our `login` mutation. */
["LoginRecord"]: AliasType<{
	couple?:ValueTypes["Pary"],
	sess?:ValueTypes["Session"],
	usr?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** All input for the `login` mutation. */
["LoginInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	login:string,
	passwd:string
};
	/** The output of our `logout` mutation. */
["LogoutPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `logout` mutation. */
["LogoutInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null
};
	["Upload"]: AliasType<{
	uploadUrl?:boolean,
		__typename?: boolean
}>;
	["UploadInput"]: {
	directory?:string | null,
	filename:string
}
  }

export type ModelTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: {
		/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query:ModelTypes["Query"],
	/** The root query type must be a `Node` to work well with Relay 1 mutations. This just resolves to `query`. */
	nodeId:string,
	/** Fetches an object given its globally unique `ID`. */
	node?:ModelTypes["Node"],
	/** Reads and enables pagination through a set of `Akce`. */
	akces?:ModelTypes["AkcesConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItems?:ModelTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualities?:ModelTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumenties?:ModelTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieDir`. */
	galerieDirs?:ModelTypes["GalerieDirsConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotos?:ModelTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `Member`. */
	members?:ModelTypes["MembersConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkas?:ModelTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItems?:ModelTypes["NabidkaItemsConnection"],
	/** Reads and enables pagination through a set of `Page`. */
	pages?:ModelTypes["PagesConnection"],
	/** Reads and enables pagination through a set of `PageRevision`. */
	pageRevisions?:ModelTypes["PageRevisionsConnection"],
	/** Reads and enables pagination through a set of `Parameter`. */
	parameters?:ModelTypes["ParametersConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	paries?:ModelTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhs?:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Permission`. */
	permissions?:ModelTypes["PermissionsConnection"],
	/** Reads and enables pagination through a set of `PlatbyCategory`. */
	platbyCategories?:ModelTypes["PlatbyCategoriesConnection"],
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroups?:ModelTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroup`. */
	platbyGroups?:ModelTypes["PlatbyGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinas?:ModelTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItems?:ModelTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `PlatbyRaw`. */
	platbyRaws?:ModelTypes["PlatbyRawsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpis?:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItems?:ModelTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessions?:ModelTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupinies?:ModelTypes["SkupiniesConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenis?:ModelTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupinies?:ModelTypes["UpozorneniSkupiniesConnection"],
	/** Reads and enables pagination through a set of `User`. */
	users?:ModelTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `UsersSkupiny`. */
	usersSkupinies?:ModelTypes["UsersSkupiniesConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	videos?:ModelTypes["VideosConnection"],
	/** Reads and enables pagination through a set of `VideoList`. */
	videoLists?:ModelTypes["VideoListsConnection"],
	/** Reads and enables pagination through a set of `VideoSource`. */
	videoSources?:ModelTypes["VideoSourcesConnection"],
	akce?:ModelTypes["Akce"],
	akceItem?:ModelTypes["AkceItem"],
	aktuality?:ModelTypes["Aktuality"],
	dokumenty?:ModelTypes["Dokumenty"],
	galerieDir?:ModelTypes["GalerieDir"],
	galerieFoto?:ModelTypes["GalerieFoto"],
	nabidka?:ModelTypes["Nabidka"],
	nabidkaItem?:ModelTypes["NabidkaItem"],
	page?:ModelTypes["Page"],
	pageByUrl?:ModelTypes["Page"],
	pageRevision?:ModelTypes["PageRevision"],
	parameter?:ModelTypes["Parameter"],
	pary?:ModelTypes["Pary"],
	paryNavrh?:ModelTypes["ParyNavrh"],
	permission?:ModelTypes["Permission"],
	platbyCategory?:ModelTypes["PlatbyCategory"],
	platbyCategoryGroup?:ModelTypes["PlatbyCategoryGroup"],
	platbyGroup?:ModelTypes["PlatbyGroup"],
	platbyGroupSkupina?:ModelTypes["PlatbyGroupSkupina"],
	platbyItem?:ModelTypes["PlatbyItem"],
	platbyRaw?:ModelTypes["PlatbyRaw"],
	rozpi?:ModelTypes["Rozpi"],
	rozpisItem?:ModelTypes["RozpisItem"],
	session?:ModelTypes["Session"],
	skupiny?:ModelTypes["Skupiny"],
	upozorneni?:ModelTypes["Upozorneni"],
	upozorneniSkupiny?:ModelTypes["UpozorneniSkupiny"],
	user?:ModelTypes["User"],
	usersSkupiny?:ModelTypes["UsersSkupiny"],
	video?:ModelTypes["Video"],
	videoList?:ModelTypes["VideoList"],
	videoSource?:ModelTypes["VideoSource"],
	currentCoupleIds?:ModelTypes["CurrentCoupleIdsConnection"],
	currentSessionId?:string,
	currentUserId?:ModelTypes["BigInt"],
	getCurrentUser?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	reservationsForRange?:ModelTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	schedulesForRange?:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	titleVideos?:ModelTypes["VideosConnection"],
	/** Reads a single `Akce` using its globally unique `ID`. */
	akceByNodeId?:ModelTypes["Akce"],
	/** Reads a single `AkceItem` using its globally unique `ID`. */
	akceItemByNodeId?:ModelTypes["AkceItem"],
	/** Reads a single `Aktuality` using its globally unique `ID`. */
	aktualityByNodeId?:ModelTypes["Aktuality"],
	/** Reads a single `Dokumenty` using its globally unique `ID`. */
	dokumentyByNodeId?:ModelTypes["Dokumenty"],
	/** Reads a single `GalerieDir` using its globally unique `ID`. */
	galerieDirByNodeId?:ModelTypes["GalerieDir"],
	/** Reads a single `GalerieFoto` using its globally unique `ID`. */
	galerieFotoByNodeId?:ModelTypes["GalerieFoto"],
	/** Reads a single `Nabidka` using its globally unique `ID`. */
	nabidkaByNodeId?:ModelTypes["Nabidka"],
	/** Reads a single `NabidkaItem` using its globally unique `ID`. */
	nabidkaItemByNodeId?:ModelTypes["NabidkaItem"],
	/** Reads a single `Page` using its globally unique `ID`. */
	pageByNodeId?:ModelTypes["Page"],
	/** Reads a single `PageRevision` using its globally unique `ID`. */
	pageRevisionByNodeId?:ModelTypes["PageRevision"],
	/** Reads a single `Parameter` using its globally unique `ID`. */
	parameterByNodeId?:ModelTypes["Parameter"],
	/** Reads a single `Pary` using its globally unique `ID`. */
	paryByNodeId?:ModelTypes["Pary"],
	/** Reads a single `ParyNavrh` using its globally unique `ID`. */
	paryNavrhByNodeId?:ModelTypes["ParyNavrh"],
	/** Reads a single `Permission` using its globally unique `ID`. */
	permissionByNodeId?:ModelTypes["Permission"],
	/** Reads a single `PlatbyCategory` using its globally unique `ID`. */
	platbyCategoryByNodeId?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyCategoryGroup` using its globally unique `ID`. */
	platbyCategoryGroupByNodeId?:ModelTypes["PlatbyCategoryGroup"],
	/** Reads a single `PlatbyGroup` using its globally unique `ID`. */
	platbyGroupByNodeId?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyGroupSkupina` using its globally unique `ID`. */
	platbyGroupSkupinaByNodeId?:ModelTypes["PlatbyGroupSkupina"],
	/** Reads a single `PlatbyItem` using its globally unique `ID`. */
	platbyItemByNodeId?:ModelTypes["PlatbyItem"],
	/** Reads a single `PlatbyRaw` using its globally unique `ID`. */
	platbyRawByNodeId?:ModelTypes["PlatbyRaw"],
	/** Reads a single `Rozpi` using its globally unique `ID`. */
	rozpiByNodeId?:ModelTypes["Rozpi"],
	/** Reads a single `RozpisItem` using its globally unique `ID`. */
	rozpisItemByNodeId?:ModelTypes["RozpisItem"],
	/** Reads a single `Session` using its globally unique `ID`. */
	sessionByNodeId?:ModelTypes["Session"],
	/** Reads a single `Skupiny` using its globally unique `ID`. */
	skupinyByNodeId?:ModelTypes["Skupiny"],
	/** Reads a single `Upozorneni` using its globally unique `ID`. */
	upozorneniByNodeId?:ModelTypes["Upozorneni"],
	/** Reads a single `UpozorneniSkupiny` using its globally unique `ID`. */
	upozorneniSkupinyByNodeId?:ModelTypes["UpozorneniSkupiny"],
	/** Reads a single `User` using its globally unique `ID`. */
	userByNodeId?:ModelTypes["User"],
	/** Reads a single `UsersSkupiny` using its globally unique `ID`. */
	usersSkupinyByNodeId?:ModelTypes["UsersSkupiny"],
	/** Reads a single `Video` using its globally unique `ID`. */
	videoByNodeId?:ModelTypes["Video"],
	/** Reads a single `VideoList` using its globally unique `ID`. */
	videoListByNodeId?:ModelTypes["VideoList"],
	/** Reads a single `VideoSource` using its globally unique `ID`. */
	videoSourceByNodeId?:ModelTypes["VideoSource"]
};
	/** An object with a globally unique `ID`. */
["Node"]: ModelTypes["Query"] | ModelTypes["Akce"] | ModelTypes["AkceItem"] | ModelTypes["User"] | ModelTypes["Permission"] | ModelTypes["Skupiny"] | ModelTypes["PlatbyGroupSkupina"] | ModelTypes["PlatbyGroup"] | ModelTypes["PlatbyCategoryGroup"] | ModelTypes["PlatbyCategory"] | ModelTypes["PlatbyItem"] | ModelTypes["PlatbyRaw"] | ModelTypes["UpozorneniSkupiny"] | ModelTypes["Upozorneni"] | ModelTypes["Aktuality"] | ModelTypes["GalerieFoto"] | ModelTypes["GalerieDir"] | ModelTypes["Nabidka"] | ModelTypes["NabidkaItem"] | ModelTypes["Pary"] | ModelTypes["RozpisItem"] | ModelTypes["Rozpi"] | ModelTypes["Session"] | ModelTypes["Dokumenty"] | ModelTypes["ParyNavrh"] | ModelTypes["Page"] | ModelTypes["PageRevision"] | ModelTypes["Parameter"] | ModelTypes["UsersSkupiny"] | ModelTypes["Video"] | ModelTypes["VideoList"] | ModelTypes["VideoSource"];
	/** A connection to a list of `Akce` values. */
["AkcesConnection"]: {
		/** A list of `Akce` objects. */
	nodes:ModelTypes["Akce"][],
	/** A list of edges which contains the `Akce` and cursor to aid in pagination. */
	edges:ModelTypes["AkcesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Akce` you could get from the connection. */
	totalCount:number
};
	["Akce"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	aId:ModelTypes["BigInt"],
	aJmeno:string,
	aKde:string,
	aInfo:string,
	aOd:ModelTypes["Date"],
	aDo:ModelTypes["Date"],
	aKapacita:ModelTypes["BigInt"],
	aDokumenty:string,
	aTimestamp?:ModelTypes["Datetime"],
	aLock:boolean,
	aVisible:boolean,
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiIdRodic:ModelTypes["AkceItemsConnection"]
};
	/** A signed eight-byte integer. The upper big integer values are greater than the
max value for a JavaScript number. Therefore all big integers will be output as
strings and not numbers. */
["BigInt"]:any;
	/** The day, does not include a time. */
["Date"]:any;
	/** A point in time as described by the [ISO
8601](https://en.wikipedia.org/wiki/ISO_8601) standard. May or may not include a timezone. */
["Datetime"]:any;
	/** A connection to a list of `AkceItem` values. */
["AkceItemsConnection"]: {
		/** A list of `AkceItem` objects. */
	nodes:ModelTypes["AkceItem"][],
	/** A list of edges which contains the `AkceItem` and cursor to aid in pagination. */
	edges:ModelTypes["AkceItemsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `AkceItem` you could get from the connection. */
	totalCount:number
};
	["AkceItem"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	aiId:ModelTypes["BigInt"],
	aiIdRodic:ModelTypes["BigInt"],
	aiUser:ModelTypes["BigInt"],
	aiRokNarozeni:number,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ModelTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ModelTypes["User"]
};
	["User"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	uId:ModelTypes["BigInt"],
	uLogin:string,
	uPass:string,
	uJmeno:string,
	uPrijmeni:string,
	uPohlavi:string,
	uEmail:string,
	uTelefon:string,
	uNarozeni:ModelTypes["Date"],
	uRodneCislo?:string,
	uPoznamky:string,
	uTimestamp:ModelTypes["Datetime"],
	uLevel:number,
	uGroup:ModelTypes["BigInt"],
	uSkupina:ModelTypes["BigInt"],
	uDancer:boolean,
	uBan:boolean,
	uLock:boolean,
	uConfirmed:boolean,
	uSystem:boolean,
	uStreet:string,
	uConscriptionNumber:string,
	uOrientationNumber:string,
	uDistrict:string,
	uCity:string,
	uPostalCode:string,
	uNationality:string,
	uMemberSince?:ModelTypes["Datetime"],
	uMemberUntil?:ModelTypes["Datetime"],
	uCreatedAt:ModelTypes["Datetime"],
	uTeacher:boolean,
	uGdprSignedAt?:ModelTypes["Datetime"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ModelTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ModelTypes["Skupiny"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtKdo:ModelTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkasByNTrener:ModelTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpisByRTrener:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessionsBySsUser:ModelTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiUser:ModelTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumentiesByDKdo:ModelTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfKdo:ModelTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdUser:ModelTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartner:ModelTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnNavrhl:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartner:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartnerka:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenisByUpKdo:ModelTypes["UpozornenisConnection"]
};
	["Permission"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	peId:ModelTypes["BigInt"],
	peName:string,
	peDescription:string,
	peAkce:number,
	peAktuality:number,
	peAnkety:number,
	peDokumenty:number,
	peGalerie:number,
	peInzerce:number,
	peKonzole:number,
	peNabidka:number,
	peNastenka:number,
	peNovinky:number,
	pePary:number,
	pePlatby:number,
	pePermissions:number,
	peRozpis:number,
	peSkupiny:number,
	peUsers:number,
	peMain:number,
	/** Reads and enables pagination through a set of `User`. */
	usersByUGroup:ModelTypes["UsersConnection"]
};
	/** A connection to a list of `User` values. */
["UsersConnection"]: {
		/** A list of `User` objects. */
	nodes:ModelTypes["User"][],
	/** A list of edges which contains the `User` and cursor to aid in pagination. */
	edges:ModelTypes["UsersEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `User` you could get from the connection. */
	totalCount:number
};
	/** A `User` edge in the connection. */
["UsersEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `User` at the end of the edge. */
	node:ModelTypes["User"]
};
	/** A location in a connection that can be used for resuming pagination. */
["Cursor"]:any;
	/** Information about pagination in a connection. */
["PageInfo"]: {
		/** When paginating forwards, are there more items? */
	hasNextPage:boolean,
	/** When paginating backwards, are there more items? */
	hasPreviousPage:boolean,
	/** When paginating backwards, the cursor to continue. */
	startCursor?:ModelTypes["Cursor"],
	/** When paginating forwards, the cursor to continue. */
	endCursor?:ModelTypes["Cursor"]
};
	/** Methods to use when ordering `User`. */
["UsersOrderBy"]: GraphQLTypes["UsersOrderBy"];
	/** A condition to be used against `User` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["UserCondition"]: GraphQLTypes["UserCondition"];
	["Skupiny"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	sId:ModelTypes["BigInt"],
	sName:string,
	sDescription:string,
	sColorRgb:string,
	sColorText:string,
	sLocation:string,
	sVisible:boolean,
	/** Reads and enables pagination through a set of `User`. */
	usersByUSkupina:ModelTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdSkupina:ModelTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdSkupina:ModelTypes["UpozorneniSkupiniesConnection"]
};
	/** A connection to a list of `PlatbyGroupSkupina` values. */
["PlatbyGroupSkupinasConnection"]: {
		/** A list of `PlatbyGroupSkupina` objects. */
	nodes:ModelTypes["PlatbyGroupSkupina"][],
	/** A list of edges which contains the `PlatbyGroupSkupina` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyGroupSkupinasEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyGroupSkupina` you could get from the connection. */
	totalCount:number
};
	["PlatbyGroupSkupina"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pgsId:ModelTypes["BigInt"],
	pgsIdSkupina:ModelTypes["BigInt"],
	pgsIdGroup:ModelTypes["BigInt"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ModelTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ModelTypes["PlatbyGroup"]
};
	["PlatbyGroup"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pgId:ModelTypes["BigInt"],
	pgType:ModelTypes["BigFloat"],
	pgName:string,
	pgDescription:string,
	pgBase:ModelTypes["BigInt"],
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroupsByPcgIdGroup:ModelTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdGroup:ModelTypes["PlatbyGroupSkupinasConnection"]
};
	/** A floating point number that requires more precision than IEEE 754 binary 64 */
["BigFloat"]:any;
	/** A connection to a list of `PlatbyCategoryGroup` values. */
["PlatbyCategoryGroupsConnection"]: {
		/** A list of `PlatbyCategoryGroup` objects. */
	nodes:ModelTypes["PlatbyCategoryGroup"][],
	/** A list of edges which contains the `PlatbyCategoryGroup` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyCategoryGroupsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyCategoryGroup` you could get from the connection. */
	totalCount:number
};
	["PlatbyCategoryGroup"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pcgId:ModelTypes["BigInt"],
	pcgIdGroup:ModelTypes["BigInt"],
	pcgIdCategory:ModelTypes["BigInt"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ModelTypes["PlatbyCategory"]
};
	["PlatbyCategory"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pcId:ModelTypes["BigInt"],
	pcName:string,
	pcSymbol:ModelTypes["BigInt"],
	pcAmount:ModelTypes["BigFloat"],
	pcDateDue:ModelTypes["Date"],
	pcValidFrom:ModelTypes["Date"],
	pcValidTo:ModelTypes["Date"],
	pcUseBase:boolean,
	pcUsePrefix:boolean,
	pcArchive:boolean,
	pcVisible:boolean,
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroupsByPcgIdCategory:ModelTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdCategory:ModelTypes["PlatbyItemsConnection"]
};
	/** Methods to use when ordering `PlatbyCategoryGroup`. */
["PlatbyCategoryGroupsOrderBy"]: GraphQLTypes["PlatbyCategoryGroupsOrderBy"];
	/** A condition to be used against `PlatbyCategoryGroup` object types. All fields
are tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryGroupCondition"]: GraphQLTypes["PlatbyCategoryGroupCondition"];
	/** A connection to a list of `PlatbyItem` values. */
["PlatbyItemsConnection"]: {
		/** A list of `PlatbyItem` objects. */
	nodes:ModelTypes["PlatbyItem"][],
	/** A list of edges which contains the `PlatbyItem` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyItemsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyItem` you could get from the connection. */
	totalCount:number
};
	["PlatbyItem"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	piId:ModelTypes["BigInt"],
	piIdUser?:ModelTypes["BigInt"],
	piIdCategory:ModelTypes["BigInt"],
	piIdRaw?:ModelTypes["BigInt"],
	piAmount:ModelTypes["BigFloat"],
	piDate:ModelTypes["Date"],
	piPrefix:number,
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ModelTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ModelTypes["PlatbyRaw"]
};
	["PlatbyRaw"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	prId:ModelTypes["BigInt"],
	prRaw:string,
	prHash:string,
	prSorted:boolean,
	prDiscarded:boolean,
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdRaw:ModelTypes["PlatbyItemsConnection"]
};
	/** Methods to use when ordering `PlatbyItem`. */
["PlatbyItemsOrderBy"]: GraphQLTypes["PlatbyItemsOrderBy"];
	/** A condition to be used against `PlatbyItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyItemCondition"]: GraphQLTypes["PlatbyItemCondition"];
	/** A `PlatbyItem` edge in the connection. */
["PlatbyItemsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyItem` at the end of the edge. */
	node:ModelTypes["PlatbyItem"]
};
	/** A `PlatbyCategoryGroup` edge in the connection. */
["PlatbyCategoryGroupsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyCategoryGroup` at the end of the edge. */
	node:ModelTypes["PlatbyCategoryGroup"]
};
	/** Methods to use when ordering `PlatbyGroupSkupina`. */
["PlatbyGroupSkupinasOrderBy"]: GraphQLTypes["PlatbyGroupSkupinasOrderBy"];
	/** A condition to be used against `PlatbyGroupSkupina` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyGroupSkupinaCondition"]: GraphQLTypes["PlatbyGroupSkupinaCondition"];
	/** A `PlatbyGroupSkupina` edge in the connection. */
["PlatbyGroupSkupinasEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyGroupSkupina` at the end of the edge. */
	node:ModelTypes["PlatbyGroupSkupina"]
};
	/** A connection to a list of `UpozorneniSkupiny` values. */
["UpozorneniSkupiniesConnection"]: {
		/** A list of `UpozorneniSkupiny` objects. */
	nodes:ModelTypes["UpozorneniSkupiny"][],
	/** A list of edges which contains the `UpozorneniSkupiny` and cursor to aid in pagination. */
	edges:ModelTypes["UpozorneniSkupiniesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `UpozorneniSkupiny` you could get from the connection. */
	totalCount:number
};
	["UpozorneniSkupiny"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	upsId:ModelTypes["BigInt"],
	upsIdRodic:ModelTypes["BigInt"],
	upsIdSkupina:ModelTypes["BigInt"],
	upsColor:string,
	upsPopis:string,
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ModelTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ModelTypes["Skupiny"]
};
	["Upozorneni"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	upId:ModelTypes["BigInt"],
	upKdo:ModelTypes["BigInt"],
	upNadpis:string,
	upText:string,
	upBarvy:ModelTypes["BigInt"],
	upLock:boolean,
	upTimestamp?:ModelTypes["Datetime"],
	upTimestampAdd:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdRodic:ModelTypes["UpozorneniSkupiniesConnection"]
};
	/** Methods to use when ordering `UpozorneniSkupiny`. */
["UpozorneniSkupiniesOrderBy"]: GraphQLTypes["UpozorneniSkupiniesOrderBy"];
	/** A condition to be used against `UpozorneniSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UpozorneniSkupinyCondition"]: GraphQLTypes["UpozorneniSkupinyCondition"];
	/** A `UpozorneniSkupiny` edge in the connection. */
["UpozorneniSkupiniesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `UpozorneniSkupiny` at the end of the edge. */
	node:ModelTypes["UpozorneniSkupiny"]
};
	/** A connection to a list of `Aktuality` values. */
["AktualitiesConnection"]: {
		/** A list of `Aktuality` objects. */
	nodes:ModelTypes["Aktuality"][],
	/** A list of edges which contains the `Aktuality` and cursor to aid in pagination. */
	edges:ModelTypes["AktualitiesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Aktuality` you could get from the connection. */
	totalCount:number
};
	["Aktuality"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	atId:ModelTypes["BigInt"],
	atKdo:ModelTypes["BigInt"],
	atKat:string,
	atJmeno:string,
	atText:string,
	atPreview:string,
	atFoto?:ModelTypes["BigInt"],
	atFotoMain?:ModelTypes["BigInt"],
	atTimestamp?:ModelTypes["Datetime"],
	atTimestampAdd?:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ModelTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ModelTypes["GalerieFoto"]
};
	["GalerieFoto"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	gfId:ModelTypes["BigInt"],
	gfIdRodic:ModelTypes["BigInt"],
	gfName:string,
	gfPath:string,
	gfKdo:ModelTypes["BigInt"],
	gfTimestamp?:ModelTypes["Datetime"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ModelTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtFotoMain:ModelTypes["AktualitiesConnection"]
};
	["GalerieDir"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	gdId:ModelTypes["BigInt"],
	gdIdRodic:ModelTypes["BigInt"],
	gdName:string,
	gdLevel:number,
	gdPath:string,
	gdHidden:boolean,
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfIdRodic:ModelTypes["GalerieFotosConnection"]
};
	/** A connection to a list of `GalerieFoto` values. */
["GalerieFotosConnection"]: {
		/** A list of `GalerieFoto` objects. */
	nodes:ModelTypes["GalerieFoto"][],
	/** A list of edges which contains the `GalerieFoto` and cursor to aid in pagination. */
	edges:ModelTypes["GalerieFotosEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `GalerieFoto` you could get from the connection. */
	totalCount:number
};
	/** A `GalerieFoto` edge in the connection. */
["GalerieFotosEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `GalerieFoto` at the end of the edge. */
	node:ModelTypes["GalerieFoto"]
};
	/** Methods to use when ordering `GalerieFoto`. */
["GalerieFotosOrderBy"]: GraphQLTypes["GalerieFotosOrderBy"];
	/** A condition to be used against `GalerieFoto` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieFotoCondition"]: GraphQLTypes["GalerieFotoCondition"];
	/** Methods to use when ordering `Aktuality`. */
["AktualitiesOrderBy"]: GraphQLTypes["AktualitiesOrderBy"];
	/** A condition to be used against `Aktuality` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AktualityCondition"]: GraphQLTypes["AktualityCondition"];
	/** A `Aktuality` edge in the connection. */
["AktualitiesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Aktuality` at the end of the edge. */
	node:ModelTypes["Aktuality"]
};
	/** A connection to a list of `Nabidka` values. */
["NabidkasConnection"]: {
		/** A list of `Nabidka` objects. */
	nodes:ModelTypes["Nabidka"][],
	/** A list of edges which contains the `Nabidka` and cursor to aid in pagination. */
	edges:ModelTypes["NabidkasEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Nabidka` you could get from the connection. */
	totalCount:number
};
	["Nabidka"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	nId:ModelTypes["BigInt"],
	nTrener:ModelTypes["BigInt"],
	nPocetHod:number,
	nMaxPocetHod:ModelTypes["BigInt"],
	nOd:ModelTypes["Date"],
	nDo:ModelTypes["Date"],
	nVisible:boolean,
	nLock:boolean,
	nTimestamp?:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiIdRodic:ModelTypes["NabidkaItemsConnection"]
};
	/** A connection to a list of `NabidkaItem` values. */
["NabidkaItemsConnection"]: {
		/** A list of `NabidkaItem` objects. */
	nodes:ModelTypes["NabidkaItem"][],
	/** A list of edges which contains the `NabidkaItem` and cursor to aid in pagination. */
	edges:ModelTypes["NabidkaItemsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `NabidkaItem` you could get from the connection. */
	totalCount:number
};
	["NabidkaItem"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	niId:ModelTypes["BigInt"],
	niIdRodic:ModelTypes["BigInt"],
	niPartner:ModelTypes["BigInt"],
	niPocetHod:number,
	niLock:boolean,
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ModelTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ModelTypes["Pary"]
};
	["Pary"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pId:ModelTypes["BigInt"],
	pIdPartner:ModelTypes["BigInt"],
	pIdPartnerka?:ModelTypes["BigInt"],
	pSttTrida:ModelTypes["ParyPSttTrida"],
	pSttBody:number,
	pSttFinale:boolean,
	pLatTrida:ModelTypes["ParyPLatTrida"],
	pLatBody:number,
	pLatFinale:boolean,
	pHodnoceni:number,
	pArchiv:boolean,
	pTimestampAdd:ModelTypes["Datetime"],
	pTimestampArchive?:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiPartner:ModelTypes["NabidkaItemsConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItemsByRiPartner:ModelTypes["RozpisItemsConnection"]
};
	["ParyPSttTrida"]: GraphQLTypes["ParyPSttTrida"];
	["ParyPLatTrida"]: GraphQLTypes["ParyPLatTrida"];
	/** Methods to use when ordering `NabidkaItem`. */
["NabidkaItemsOrderBy"]: GraphQLTypes["NabidkaItemsOrderBy"];
	/** A condition to be used against `NabidkaItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["NabidkaItemCondition"]: GraphQLTypes["NabidkaItemCondition"];
	/** A connection to a list of `RozpisItem` values. */
["RozpisItemsConnection"]: {
		/** A list of `RozpisItem` objects. */
	nodes:ModelTypes["RozpisItem"][],
	/** A list of edges which contains the `RozpisItem` and cursor to aid in pagination. */
	edges:ModelTypes["RozpisItemsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `RozpisItem` you could get from the connection. */
	totalCount:number
};
	["RozpisItem"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	riId:ModelTypes["BigInt"],
	riIdRodic:ModelTypes["BigInt"],
	riPartner?:ModelTypes["BigInt"],
	riOd:ModelTypes["Time"],
	riDo:ModelTypes["Time"],
	riLock:boolean,
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ModelTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ModelTypes["Pary"]
};
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:any;
	["Rozpi"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	rId:ModelTypes["BigInt"],
	rTrener:ModelTypes["BigInt"],
	rKde:string,
	rDatum:ModelTypes["Date"],
	rVisible:boolean,
	rLock:boolean,
	rTimestamp?:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItemsByRiIdRodic:ModelTypes["RozpisItemsConnection"]
};
	/** Methods to use when ordering `RozpisItem`. */
["RozpisItemsOrderBy"]: GraphQLTypes["RozpisItemsOrderBy"];
	/** A condition to be used against `RozpisItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["RozpisItemCondition"]: GraphQLTypes["RozpisItemCondition"];
	/** A `RozpisItem` edge in the connection. */
["RozpisItemsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `RozpisItem` at the end of the edge. */
	node:ModelTypes["RozpisItem"]
};
	/** A `NabidkaItem` edge in the connection. */
["NabidkaItemsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `NabidkaItem` at the end of the edge. */
	node:ModelTypes["NabidkaItem"]
};
	/** A `Nabidka` edge in the connection. */
["NabidkasEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Nabidka` at the end of the edge. */
	node:ModelTypes["Nabidka"]
};
	/** Methods to use when ordering `Nabidka`. */
["NabidkasOrderBy"]: GraphQLTypes["NabidkasOrderBy"];
	/** A condition to be used against `Nabidka` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["NabidkaCondition"]: GraphQLTypes["NabidkaCondition"];
	/** A connection to a list of `Rozpi` values. */
["RozpisConnection"]: {
		/** A list of `Rozpi` objects. */
	nodes:ModelTypes["Rozpi"][],
	/** A list of edges which contains the `Rozpi` and cursor to aid in pagination. */
	edges:ModelTypes["RozpisEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Rozpi` you could get from the connection. */
	totalCount:number
};
	/** A `Rozpi` edge in the connection. */
["RozpisEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Rozpi` at the end of the edge. */
	node:ModelTypes["Rozpi"]
};
	/** Methods to use when ordering `Rozpi`. */
["RozpisOrderBy"]: GraphQLTypes["RozpisOrderBy"];
	/** A condition to be used against `Rozpi` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RozpiCondition"]: GraphQLTypes["RozpiCondition"];
	/** A connection to a list of `Session` values. */
["SessionsConnection"]: {
		/** A list of `Session` objects. */
	nodes:ModelTypes["Session"][],
	/** A list of edges which contains the `Session` and cursor to aid in pagination. */
	edges:ModelTypes["SessionsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Session` you could get from the connection. */
	totalCount:number
};
	["Session"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	ssId:string,
	ssData:string,
	ssUpdatedAt:ModelTypes["Datetime"],
	ssLifetime:ModelTypes["BigInt"],
	ssUser?:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ModelTypes["User"]
};
	/** A `Session` edge in the connection. */
["SessionsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Session` at the end of the edge. */
	node:ModelTypes["Session"]
};
	/** Methods to use when ordering `Session`. */
["SessionsOrderBy"]: GraphQLTypes["SessionsOrderBy"];
	/** A condition to be used against `Session` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SessionCondition"]: GraphQLTypes["SessionCondition"];
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]: GraphQLTypes["AkceItemsOrderBy"];
	/** A condition to be used against `AkceItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AkceItemCondition"]: GraphQLTypes["AkceItemCondition"];
	/** A connection to a list of `Dokumenty` values. */
["DokumentiesConnection"]: {
		/** A list of `Dokumenty` objects. */
	nodes:ModelTypes["Dokumenty"][],
	/** A list of edges which contains the `Dokumenty` and cursor to aid in pagination. */
	edges:ModelTypes["DokumentiesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Dokumenty` you could get from the connection. */
	totalCount:number
};
	["Dokumenty"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	dId:ModelTypes["BigInt"],
	dPath:string,
	dName:string,
	dFilename:string,
	dKategorie:number,
	dKdo:ModelTypes["BigInt"],
	dTimestamp?:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ModelTypes["User"]
};
	/** A `Dokumenty` edge in the connection. */
["DokumentiesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Dokumenty` at the end of the edge. */
	node:ModelTypes["Dokumenty"]
};
	/** Methods to use when ordering `Dokumenty`. */
["DokumentiesOrderBy"]: GraphQLTypes["DokumentiesOrderBy"];
	/** A condition to be used against `Dokumenty` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["DokumentyCondition"]: GraphQLTypes["DokumentyCondition"];
	/** A connection to a list of `Pary` values. */
["PariesConnection"]: {
		/** A list of `Pary` objects. */
	nodes:ModelTypes["Pary"][],
	/** A list of edges which contains the `Pary` and cursor to aid in pagination. */
	edges:ModelTypes["PariesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Pary` you could get from the connection. */
	totalCount:number
};
	/** A `Pary` edge in the connection. */
["PariesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Pary` at the end of the edge. */
	node:ModelTypes["Pary"]
};
	/** Methods to use when ordering `Pary`. */
["PariesOrderBy"]: GraphQLTypes["PariesOrderBy"];
	/** A condition to be used against `Pary` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["ParyCondition"]: GraphQLTypes["ParyCondition"];
	/** A connection to a list of `ParyNavrh` values. */
["ParyNavrhsConnection"]: {
		/** A list of `ParyNavrh` objects. */
	nodes:ModelTypes["ParyNavrh"][],
	/** A list of edges which contains the `ParyNavrh` and cursor to aid in pagination. */
	edges:ModelTypes["ParyNavrhsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `ParyNavrh` you could get from the connection. */
	totalCount:number
};
	["ParyNavrh"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	pnId:ModelTypes["BigInt"],
	pnNavrhl:ModelTypes["BigInt"],
	pnPartner:ModelTypes["BigInt"],
	pnPartnerka:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ModelTypes["User"]
};
	/** A `ParyNavrh` edge in the connection. */
["ParyNavrhsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `ParyNavrh` at the end of the edge. */
	node:ModelTypes["ParyNavrh"]
};
	/** Methods to use when ordering `ParyNavrh`. */
["ParyNavrhsOrderBy"]: GraphQLTypes["ParyNavrhsOrderBy"];
	/** A condition to be used against `ParyNavrh` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParyNavrhCondition"]: GraphQLTypes["ParyNavrhCondition"];
	/** A connection to a list of `Upozorneni` values. */
["UpozornenisConnection"]: {
		/** A list of `Upozorneni` objects. */
	nodes:ModelTypes["Upozorneni"][],
	/** A list of edges which contains the `Upozorneni` and cursor to aid in pagination. */
	edges:ModelTypes["UpozornenisEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Upozorneni` you could get from the connection. */
	totalCount:number
};
	/** A `Upozorneni` edge in the connection. */
["UpozornenisEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Upozorneni` at the end of the edge. */
	node:ModelTypes["Upozorneni"]
};
	/** Methods to use when ordering `Upozorneni`. */
["UpozornenisOrderBy"]: GraphQLTypes["UpozornenisOrderBy"];
	/** A condition to be used against `Upozorneni` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["UpozorneniCondition"]: GraphQLTypes["UpozorneniCondition"];
	/** A `AkceItem` edge in the connection. */
["AkceItemsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `AkceItem` at the end of the edge. */
	node:ModelTypes["AkceItem"]
};
	/** A `Akce` edge in the connection. */
["AkcesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Akce` at the end of the edge. */
	node:ModelTypes["Akce"]
};
	/** Methods to use when ordering `Akce`. */
["AkcesOrderBy"]: GraphQLTypes["AkcesOrderBy"];
	/** A condition to be used against `Akce` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["AkceCondition"]: GraphQLTypes["AkceCondition"];
	/** A connection to a list of `GalerieDir` values. */
["GalerieDirsConnection"]: {
		/** A list of `GalerieDir` objects. */
	nodes:ModelTypes["GalerieDir"][],
	/** A list of edges which contains the `GalerieDir` and cursor to aid in pagination. */
	edges:ModelTypes["GalerieDirsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `GalerieDir` you could get from the connection. */
	totalCount:number
};
	/** A `GalerieDir` edge in the connection. */
["GalerieDirsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `GalerieDir` at the end of the edge. */
	node:ModelTypes["GalerieDir"]
};
	/** Methods to use when ordering `GalerieDir`. */
["GalerieDirsOrderBy"]: GraphQLTypes["GalerieDirsOrderBy"];
	/** A condition to be used against `GalerieDir` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieDirCondition"]: GraphQLTypes["GalerieDirCondition"];
	/** A connection to a list of `Member` values. */
["MembersConnection"]: {
		/** A list of `Member` objects. */
	nodes:ModelTypes["Member"][],
	/** A list of edges which contains the `Member` and cursor to aid in pagination. */
	edges:ModelTypes["MembersEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Member` you could get from the connection. */
	totalCount:number
};
	["Member"]: {
		uId?:ModelTypes["BigInt"],
	uLogin?:string,
	uPass?:string,
	uJmeno?:string,
	uPrijmeni?:string,
	uPohlavi?:string,
	uEmail?:string,
	uTelefon?:string,
	uNarozeni?:ModelTypes["Date"],
	uRodneCislo?:string,
	uPoznamky?:string,
	uTimestamp?:ModelTypes["Datetime"],
	uLevel?:number,
	uGroup?:ModelTypes["BigInt"],
	uSkupina?:ModelTypes["BigInt"],
	uDancer?:boolean,
	uBan?:boolean,
	uLock?:boolean,
	uConfirmed?:boolean,
	uSystem?:boolean,
	uStreet?:string,
	uConscriptionNumber?:string,
	uOrientationNumber?:string,
	uDistrict?:string,
	uCity?:string,
	uPostalCode?:string,
	uNationality?:string,
	uMemberSince?:ModelTypes["Datetime"],
	uMemberUntil?:ModelTypes["Datetime"],
	uCreatedAt?:ModelTypes["Datetime"],
	uTeacher?:boolean,
	uGdprSignedAt?:ModelTypes["Datetime"],
	sId?:ModelTypes["BigInt"],
	sName?:string,
	paymentValid?:boolean
};
	/** A `Member` edge in the connection. */
["MembersEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Member` at the end of the edge. */
	node:ModelTypes["Member"]
};
	/** Methods to use when ordering `Member`. */
["MembersOrderBy"]: GraphQLTypes["MembersOrderBy"];
	/** A condition to be used against `Member` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["MemberCondition"]: GraphQLTypes["MemberCondition"];
	/** A connection to a list of `Page` values. */
["PagesConnection"]: {
		/** A list of `Page` objects. */
	nodes:ModelTypes["Page"][],
	/** A list of edges which contains the `Page` and cursor to aid in pagination. */
	edges:ModelTypes["PagesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Page` you could get from the connection. */
	totalCount:number
};
	["Page"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	id:number,
	url:string,
	content:ModelTypes["JSON"],
	createdAt:ModelTypes["Datetime"],
	updatedAt:ModelTypes["Datetime"],
	title:string
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	/** A `Page` edge in the connection. */
["PagesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Page` at the end of the edge. */
	node:ModelTypes["Page"]
};
	/** Methods to use when ordering `Page`. */
["PagesOrderBy"]: GraphQLTypes["PagesOrderBy"];
	/** A condition to be used against `Page` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PageCondition"]: GraphQLTypes["PageCondition"];
	/** A connection to a list of `PageRevision` values. */
["PageRevisionsConnection"]: {
		/** A list of `PageRevision` objects. */
	nodes:ModelTypes["PageRevision"][],
	/** A list of edges which contains the `PageRevision` and cursor to aid in pagination. */
	edges:ModelTypes["PageRevisionsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PageRevision` you could get from the connection. */
	totalCount:number
};
	["PageRevision"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	revNumber:number,
	revOperation:string,
	revTimestamp?:ModelTypes["Datetime"],
	id:number,
	url:string,
	content:ModelTypes["JSON"],
	createdAt:ModelTypes["Datetime"],
	updatedAt:ModelTypes["Datetime"],
	title:string
};
	/** A `PageRevision` edge in the connection. */
["PageRevisionsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PageRevision` at the end of the edge. */
	node:ModelTypes["PageRevision"]
};
	/** Methods to use when ordering `PageRevision`. */
["PageRevisionsOrderBy"]: GraphQLTypes["PageRevisionsOrderBy"];
	/** A condition to be used against `PageRevision` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PageRevisionCondition"]: GraphQLTypes["PageRevisionCondition"];
	/** A connection to a list of `Parameter` values. */
["ParametersConnection"]: {
		/** A list of `Parameter` objects. */
	nodes:ModelTypes["Parameter"][],
	/** A list of edges which contains the `Parameter` and cursor to aid in pagination. */
	edges:ModelTypes["ParametersEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Parameter` you could get from the connection. */
	totalCount:number
};
	["Parameter"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	paName:string,
	paValue:string
};
	/** A `Parameter` edge in the connection. */
["ParametersEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Parameter` at the end of the edge. */
	node:ModelTypes["Parameter"]
};
	/** Methods to use when ordering `Parameter`. */
["ParametersOrderBy"]: GraphQLTypes["ParametersOrderBy"];
	/** A condition to be used against `Parameter` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParameterCondition"]: GraphQLTypes["ParameterCondition"];
	/** A connection to a list of `Permission` values. */
["PermissionsConnection"]: {
		/** A list of `Permission` objects. */
	nodes:ModelTypes["Permission"][],
	/** A list of edges which contains the `Permission` and cursor to aid in pagination. */
	edges:ModelTypes["PermissionsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Permission` you could get from the connection. */
	totalCount:number
};
	/** A `Permission` edge in the connection. */
["PermissionsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Permission` at the end of the edge. */
	node:ModelTypes["Permission"]
};
	/** Methods to use when ordering `Permission`. */
["PermissionsOrderBy"]: GraphQLTypes["PermissionsOrderBy"];
	/** A condition to be used against `Permission` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PermissionCondition"]: GraphQLTypes["PermissionCondition"];
	/** A connection to a list of `PlatbyCategory` values. */
["PlatbyCategoriesConnection"]: {
		/** A list of `PlatbyCategory` objects. */
	nodes:ModelTypes["PlatbyCategory"][],
	/** A list of edges which contains the `PlatbyCategory` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyCategoriesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyCategory` you could get from the connection. */
	totalCount:number
};
	/** A `PlatbyCategory` edge in the connection. */
["PlatbyCategoriesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyCategory` at the end of the edge. */
	node:ModelTypes["PlatbyCategory"]
};
	/** Methods to use when ordering `PlatbyCategory`. */
["PlatbyCategoriesOrderBy"]: GraphQLTypes["PlatbyCategoriesOrderBy"];
	/** A condition to be used against `PlatbyCategory` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryCondition"]: GraphQLTypes["PlatbyCategoryCondition"];
	/** A connection to a list of `PlatbyGroup` values. */
["PlatbyGroupsConnection"]: {
		/** A list of `PlatbyGroup` objects. */
	nodes:ModelTypes["PlatbyGroup"][],
	/** A list of edges which contains the `PlatbyGroup` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyGroupsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyGroup` you could get from the connection. */
	totalCount:number
};
	/** A `PlatbyGroup` edge in the connection. */
["PlatbyGroupsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyGroup` at the end of the edge. */
	node:ModelTypes["PlatbyGroup"]
};
	/** Methods to use when ordering `PlatbyGroup`. */
["PlatbyGroupsOrderBy"]: GraphQLTypes["PlatbyGroupsOrderBy"];
	/** A condition to be used against `PlatbyGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyGroupCondition"]: GraphQLTypes["PlatbyGroupCondition"];
	/** A connection to a list of `PlatbyRaw` values. */
["PlatbyRawsConnection"]: {
		/** A list of `PlatbyRaw` objects. */
	nodes:ModelTypes["PlatbyRaw"][],
	/** A list of edges which contains the `PlatbyRaw` and cursor to aid in pagination. */
	edges:ModelTypes["PlatbyRawsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `PlatbyRaw` you could get from the connection. */
	totalCount:number
};
	/** A `PlatbyRaw` edge in the connection. */
["PlatbyRawsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `PlatbyRaw` at the end of the edge. */
	node:ModelTypes["PlatbyRaw"]
};
	/** Methods to use when ordering `PlatbyRaw`. */
["PlatbyRawsOrderBy"]: GraphQLTypes["PlatbyRawsOrderBy"];
	/** A condition to be used against `PlatbyRaw` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyRawCondition"]: GraphQLTypes["PlatbyRawCondition"];
	/** A connection to a list of `Skupiny` values. */
["SkupiniesConnection"]: {
		/** A list of `Skupiny` objects. */
	nodes:ModelTypes["Skupiny"][],
	/** A list of edges which contains the `Skupiny` and cursor to aid in pagination. */
	edges:ModelTypes["SkupiniesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Skupiny` you could get from the connection. */
	totalCount:number
};
	/** A `Skupiny` edge in the connection. */
["SkupiniesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Skupiny` at the end of the edge. */
	node:ModelTypes["Skupiny"]
};
	/** Methods to use when ordering `Skupiny`. */
["SkupiniesOrderBy"]: GraphQLTypes["SkupiniesOrderBy"];
	/** A condition to be used against `Skupiny` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SkupinyCondition"]: GraphQLTypes["SkupinyCondition"];
	/** A connection to a list of `UsersSkupiny` values. */
["UsersSkupiniesConnection"]: {
		/** A list of `UsersSkupiny` objects. */
	nodes:ModelTypes["UsersSkupiny"][],
	/** A list of edges which contains the `UsersSkupiny` and cursor to aid in pagination. */
	edges:ModelTypes["UsersSkupiniesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `UsersSkupiny` you could get from the connection. */
	totalCount:number
};
	["UsersSkupiny"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	usId:ModelTypes["BigInt"],
	usColor:string,
	usPlatbaMesic:ModelTypes["BigInt"],
	usPlatbaCtvrtrok:ModelTypes["BigInt"],
	usPlatbaPulrok:ModelTypes["BigInt"],
	usPopis:string
};
	/** A `UsersSkupiny` edge in the connection. */
["UsersSkupiniesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `UsersSkupiny` at the end of the edge. */
	node:ModelTypes["UsersSkupiny"]
};
	/** Methods to use when ordering `UsersSkupiny`. */
["UsersSkupiniesOrderBy"]: GraphQLTypes["UsersSkupiniesOrderBy"];
	/** A condition to be used against `UsersSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UsersSkupinyCondition"]: GraphQLTypes["UsersSkupinyCondition"];
	/** A connection to a list of `Video` values. */
["VideosConnection"]: {
		/** A list of `Video` objects. */
	nodes:ModelTypes["Video"][],
	/** A list of edges which contains the `Video` and cursor to aid in pagination. */
	edges:ModelTypes["VideosEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Video` you could get from the connection. */
	totalCount:number
};
	["Video"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	vId:ModelTypes["BigInt"],
	vUri:string,
	vTitle:string,
	vAuthor:string,
	vDescription:string,
	vPlaylist?:string,
	vCreatedAt:ModelTypes["Datetime"],
	vUpdatedAt:ModelTypes["Datetime"]
};
	/** A `Video` edge in the connection. */
["VideosEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Video` at the end of the edge. */
	node:ModelTypes["Video"]
};
	/** Methods to use when ordering `Video`. */
["VideosOrderBy"]: GraphQLTypes["VideosOrderBy"];
	/** A condition to be used against `Video` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["VideoCondition"]: GraphQLTypes["VideoCondition"];
	/** A connection to a list of `VideoList` values. */
["VideoListsConnection"]: {
		/** A list of `VideoList` objects. */
	nodes:ModelTypes["VideoList"][],
	/** A list of edges which contains the `VideoList` and cursor to aid in pagination. */
	edges:ModelTypes["VideoListsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `VideoList` you could get from the connection. */
	totalCount:number
};
	["VideoList"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	vlId:ModelTypes["BigInt"],
	vlUrl:string,
	vlTitle:string,
	vlDescription:string,
	vlCount:ModelTypes["BigInt"],
	vlCreatedAt:ModelTypes["Datetime"],
	vlLastChecked?:ModelTypes["Datetime"]
};
	/** A `VideoList` edge in the connection. */
["VideoListsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `VideoList` at the end of the edge. */
	node:ModelTypes["VideoList"]
};
	/** Methods to use when ordering `VideoList`. */
["VideoListsOrderBy"]: GraphQLTypes["VideoListsOrderBy"];
	/** A condition to be used against `VideoList` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoListCondition"]: GraphQLTypes["VideoListCondition"];
	/** A connection to a list of `VideoSource` values. */
["VideoSourcesConnection"]: {
		/** A list of `VideoSource` objects. */
	nodes:ModelTypes["VideoSource"][],
	/** A list of edges which contains the `VideoSource` and cursor to aid in pagination. */
	edges:ModelTypes["VideoSourcesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `VideoSource` you could get from the connection. */
	totalCount:number
};
	["VideoSource"]: {
		/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId:string,
	vsId:ModelTypes["BigInt"],
	vsUrl:string,
	vsTitle?:string,
	vsDescription?:string,
	vsCreatedAt:ModelTypes["Datetime"],
	vsLastChecked?:ModelTypes["Datetime"]
};
	/** A `VideoSource` edge in the connection. */
["VideoSourcesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `VideoSource` at the end of the edge. */
	node:ModelTypes["VideoSource"]
};
	/** Methods to use when ordering `VideoSource`. */
["VideoSourcesOrderBy"]: GraphQLTypes["VideoSourcesOrderBy"];
	/** A condition to be used against `VideoSource` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoSourceCondition"]: GraphQLTypes["VideoSourceCondition"];
	/** A connection to a list of `BigInt` values. */
["CurrentCoupleIdsConnection"]: {
		/** A list of `BigInt` objects. */
	nodes?:ModelTypes["BigInt"][],
	/** A list of edges which contains the `BigInt` and cursor to aid in pagination. */
	edges:ModelTypes["CurrentCoupleIdEdge"][],
	/** The count of *all* `BigInt` you could get from the connection. */
	totalCount:number
};
	/** A `BigInt` edge in the connection. */
["CurrentCoupleIdEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `BigInt` at the end of the edge. */
	node?:ModelTypes["BigInt"]
};
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: {
		/** Creates a single `Akce`. */
	createAkce?:ModelTypes["CreateAkcePayload"],
	/** Creates a single `AkceItem`. */
	createAkceItem?:ModelTypes["CreateAkceItemPayload"],
	/** Creates a single `Aktuality`. */
	createAktuality?:ModelTypes["CreateAktualityPayload"],
	/** Creates a single `Dokumenty`. */
	createDokumenty?:ModelTypes["CreateDokumentyPayload"],
	/** Creates a single `GalerieDir`. */
	createGalerieDir?:ModelTypes["CreateGalerieDirPayload"],
	/** Creates a single `GalerieFoto`. */
	createGalerieFoto?:ModelTypes["CreateGalerieFotoPayload"],
	/** Creates a single `Nabidka`. */
	createNabidka?:ModelTypes["CreateNabidkaPayload"],
	/** Creates a single `NabidkaItem`. */
	createNabidkaItem?:ModelTypes["CreateNabidkaItemPayload"],
	/** Creates a single `Page`. */
	createPage?:ModelTypes["CreatePagePayload"],
	/** Creates a single `Parameter`. */
	createParameter?:ModelTypes["CreateParameterPayload"],
	/** Creates a single `Pary`. */
	createPary?:ModelTypes["CreateParyPayload"],
	/** Creates a single `ParyNavrh`. */
	createParyNavrh?:ModelTypes["CreateParyNavrhPayload"],
	/** Creates a single `Permission`. */
	createPermission?:ModelTypes["CreatePermissionPayload"],
	/** Creates a single `PlatbyCategory`. */
	createPlatbyCategory?:ModelTypes["CreatePlatbyCategoryPayload"],
	/** Creates a single `PlatbyCategoryGroup`. */
	createPlatbyCategoryGroup?:ModelTypes["CreatePlatbyCategoryGroupPayload"],
	/** Creates a single `PlatbyGroup`. */
	createPlatbyGroup?:ModelTypes["CreatePlatbyGroupPayload"],
	/** Creates a single `PlatbyGroupSkupina`. */
	createPlatbyGroupSkupina?:ModelTypes["CreatePlatbyGroupSkupinaPayload"],
	/** Creates a single `PlatbyItem`. */
	createPlatbyItem?:ModelTypes["CreatePlatbyItemPayload"],
	/** Creates a single `PlatbyRaw`. */
	createPlatbyRaw?:ModelTypes["CreatePlatbyRawPayload"],
	/** Creates a single `Rozpi`. */
	createRozpi?:ModelTypes["CreateRozpiPayload"],
	/** Creates a single `RozpisItem`. */
	createRozpisItem?:ModelTypes["CreateRozpisItemPayload"],
	/** Creates a single `Session`. */
	createSession?:ModelTypes["CreateSessionPayload"],
	/** Creates a single `Skupiny`. */
	createSkupiny?:ModelTypes["CreateSkupinyPayload"],
	/** Creates a single `Upozorneni`. */
	createUpozorneni?:ModelTypes["CreateUpozorneniPayload"],
	/** Creates a single `UpozorneniSkupiny`. */
	createUpozorneniSkupiny?:ModelTypes["CreateUpozorneniSkupinyPayload"],
	/** Creates a single `User`. */
	createUser?:ModelTypes["CreateUserPayload"],
	/** Creates a single `UsersSkupiny`. */
	createUsersSkupiny?:ModelTypes["CreateUsersSkupinyPayload"],
	/** Creates a single `Video`. */
	createVideo?:ModelTypes["CreateVideoPayload"],
	/** Creates a single `VideoList`. */
	createVideoList?:ModelTypes["CreateVideoListPayload"],
	/** Creates a single `VideoSource`. */
	createVideoSource?:ModelTypes["CreateVideoSourcePayload"],
	/** Updates a single `Akce` using its globally unique id and a patch. */
	updateAkceByNodeId?:ModelTypes["UpdateAkcePayload"],
	/** Updates a single `Akce` using a unique key and a patch. */
	updateAkce?:ModelTypes["UpdateAkcePayload"],
	/** Updates a single `AkceItem` using its globally unique id and a patch. */
	updateAkceItemByNodeId?:ModelTypes["UpdateAkceItemPayload"],
	/** Updates a single `AkceItem` using a unique key and a patch. */
	updateAkceItem?:ModelTypes["UpdateAkceItemPayload"],
	/** Updates a single `Aktuality` using its globally unique id and a patch. */
	updateAktualityByNodeId?:ModelTypes["UpdateAktualityPayload"],
	/** Updates a single `Aktuality` using a unique key and a patch. */
	updateAktuality?:ModelTypes["UpdateAktualityPayload"],
	/** Updates a single `Dokumenty` using its globally unique id and a patch. */
	updateDokumentyByNodeId?:ModelTypes["UpdateDokumentyPayload"],
	/** Updates a single `Dokumenty` using a unique key and a patch. */
	updateDokumenty?:ModelTypes["UpdateDokumentyPayload"],
	/** Updates a single `GalerieDir` using its globally unique id and a patch. */
	updateGalerieDirByNodeId?:ModelTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieDir` using a unique key and a patch. */
	updateGalerieDir?:ModelTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieFoto` using its globally unique id and a patch. */
	updateGalerieFotoByNodeId?:ModelTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `GalerieFoto` using a unique key and a patch. */
	updateGalerieFoto?:ModelTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `Nabidka` using its globally unique id and a patch. */
	updateNabidkaByNodeId?:ModelTypes["UpdateNabidkaPayload"],
	/** Updates a single `Nabidka` using a unique key and a patch. */
	updateNabidka?:ModelTypes["UpdateNabidkaPayload"],
	/** Updates a single `NabidkaItem` using its globally unique id and a patch. */
	updateNabidkaItemByNodeId?:ModelTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItem?:ModelTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `Page` using its globally unique id and a patch. */
	updatePageByNodeId?:ModelTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePage?:ModelTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePageByUrl?:ModelTypes["UpdatePagePayload"],
	/** Updates a single `Parameter` using its globally unique id and a patch. */
	updateParameterByNodeId?:ModelTypes["UpdateParameterPayload"],
	/** Updates a single `Parameter` using a unique key and a patch. */
	updateParameter?:ModelTypes["UpdateParameterPayload"],
	/** Updates a single `Pary` using its globally unique id and a patch. */
	updateParyByNodeId?:ModelTypes["UpdateParyPayload"],
	/** Updates a single `Pary` using a unique key and a patch. */
	updatePary?:ModelTypes["UpdateParyPayload"],
	/** Updates a single `ParyNavrh` using its globally unique id and a patch. */
	updateParyNavrhByNodeId?:ModelTypes["UpdateParyNavrhPayload"],
	/** Updates a single `ParyNavrh` using a unique key and a patch. */
	updateParyNavrh?:ModelTypes["UpdateParyNavrhPayload"],
	/** Updates a single `Permission` using its globally unique id and a patch. */
	updatePermissionByNodeId?:ModelTypes["UpdatePermissionPayload"],
	/** Updates a single `Permission` using a unique key and a patch. */
	updatePermission?:ModelTypes["UpdatePermissionPayload"],
	/** Updates a single `PlatbyCategory` using its globally unique id and a patch. */
	updatePlatbyCategoryByNodeId?:ModelTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategory` using a unique key and a patch. */
	updatePlatbyCategory?:ModelTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategoryGroup` using its globally unique id and a patch. */
	updatePlatbyCategoryGroupByNodeId?:ModelTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyCategoryGroup` using a unique key and a patch. */
	updatePlatbyCategoryGroup?:ModelTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyGroup` using its globally unique id and a patch. */
	updatePlatbyGroupByNodeId?:ModelTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroup` using a unique key and a patch. */
	updatePlatbyGroup?:ModelTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroupSkupina` using its globally unique id and a patch. */
	updatePlatbyGroupSkupinaByNodeId?:ModelTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyGroupSkupina` using a unique key and a patch. */
	updatePlatbyGroupSkupina?:ModelTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyItem` using its globally unique id and a patch. */
	updatePlatbyItemByNodeId?:ModelTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyItem` using a unique key and a patch. */
	updatePlatbyItem?:ModelTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyRaw` using its globally unique id and a patch. */
	updatePlatbyRawByNodeId?:ModelTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `PlatbyRaw` using a unique key and a patch. */
	updatePlatbyRaw?:ModelTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `Rozpi` using its globally unique id and a patch. */
	updateRozpiByNodeId?:ModelTypes["UpdateRozpiPayload"],
	/** Updates a single `Rozpi` using a unique key and a patch. */
	updateRozpi?:ModelTypes["UpdateRozpiPayload"],
	/** Updates a single `RozpisItem` using its globally unique id and a patch. */
	updateRozpisItemByNodeId?:ModelTypes["UpdateRozpisItemPayload"],
	/** Updates a single `RozpisItem` using a unique key and a patch. */
	updateRozpisItem?:ModelTypes["UpdateRozpisItemPayload"],
	/** Updates a single `Session` using its globally unique id and a patch. */
	updateSessionByNodeId?:ModelTypes["UpdateSessionPayload"],
	/** Updates a single `Session` using a unique key and a patch. */
	updateSession?:ModelTypes["UpdateSessionPayload"],
	/** Updates a single `Skupiny` using its globally unique id and a patch. */
	updateSkupinyByNodeId?:ModelTypes["UpdateSkupinyPayload"],
	/** Updates a single `Skupiny` using a unique key and a patch. */
	updateSkupiny?:ModelTypes["UpdateSkupinyPayload"],
	/** Updates a single `Upozorneni` using its globally unique id and a patch. */
	updateUpozorneniByNodeId?:ModelTypes["UpdateUpozorneniPayload"],
	/** Updates a single `Upozorneni` using a unique key and a patch. */
	updateUpozorneni?:ModelTypes["UpdateUpozorneniPayload"],
	/** Updates a single `UpozorneniSkupiny` using its globally unique id and a patch. */
	updateUpozorneniSkupinyByNodeId?:ModelTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `UpozorneniSkupiny` using a unique key and a patch. */
	updateUpozorneniSkupiny?:ModelTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `User` using its globally unique id and a patch. */
	updateUserByNodeId?:ModelTypes["UpdateUserPayload"],
	/** Updates a single `User` using a unique key and a patch. */
	updateUser?:ModelTypes["UpdateUserPayload"],
	/** Updates a single `UsersSkupiny` using its globally unique id and a patch. */
	updateUsersSkupinyByNodeId?:ModelTypes["UpdateUsersSkupinyPayload"],
	/** Updates a single `UsersSkupiny` using a unique key and a patch. */
	updateUsersSkupiny?:ModelTypes["UpdateUsersSkupinyPayload"],
	/** Updates a single `Video` using its globally unique id and a patch. */
	updateVideoByNodeId?:ModelTypes["UpdateVideoPayload"],
	/** Updates a single `Video` using a unique key and a patch. */
	updateVideo?:ModelTypes["UpdateVideoPayload"],
	/** Updates a single `VideoList` using its globally unique id and a patch. */
	updateVideoListByNodeId?:ModelTypes["UpdateVideoListPayload"],
	/** Updates a single `VideoList` using a unique key and a patch. */
	updateVideoList?:ModelTypes["UpdateVideoListPayload"],
	/** Updates a single `VideoSource` using its globally unique id and a patch. */
	updateVideoSourceByNodeId?:ModelTypes["UpdateVideoSourcePayload"],
	/** Updates a single `VideoSource` using a unique key and a patch. */
	updateVideoSource?:ModelTypes["UpdateVideoSourcePayload"],
	/** Deletes a single `Akce` using its globally unique id. */
	deleteAkceByNodeId?:ModelTypes["DeleteAkcePayload"],
	/** Deletes a single `Akce` using a unique key. */
	deleteAkce?:ModelTypes["DeleteAkcePayload"],
	/** Deletes a single `AkceItem` using its globally unique id. */
	deleteAkceItemByNodeId?:ModelTypes["DeleteAkceItemPayload"],
	/** Deletes a single `AkceItem` using a unique key. */
	deleteAkceItem?:ModelTypes["DeleteAkceItemPayload"],
	/** Deletes a single `Aktuality` using its globally unique id. */
	deleteAktualityByNodeId?:ModelTypes["DeleteAktualityPayload"],
	/** Deletes a single `Aktuality` using a unique key. */
	deleteAktuality?:ModelTypes["DeleteAktualityPayload"],
	/** Deletes a single `Dokumenty` using its globally unique id. */
	deleteDokumentyByNodeId?:ModelTypes["DeleteDokumentyPayload"],
	/** Deletes a single `Dokumenty` using a unique key. */
	deleteDokumenty?:ModelTypes["DeleteDokumentyPayload"],
	/** Deletes a single `GalerieDir` using its globally unique id. */
	deleteGalerieDirByNodeId?:ModelTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieDir` using a unique key. */
	deleteGalerieDir?:ModelTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieFoto` using its globally unique id. */
	deleteGalerieFotoByNodeId?:ModelTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `GalerieFoto` using a unique key. */
	deleteGalerieFoto?:ModelTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `Nabidka` using its globally unique id. */
	deleteNabidkaByNodeId?:ModelTypes["DeleteNabidkaPayload"],
	/** Deletes a single `Nabidka` using a unique key. */
	deleteNabidka?:ModelTypes["DeleteNabidkaPayload"],
	/** Deletes a single `NabidkaItem` using its globally unique id. */
	deleteNabidkaItemByNodeId?:ModelTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItem?:ModelTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `Parameter` using its globally unique id. */
	deleteParameterByNodeId?:ModelTypes["DeleteParameterPayload"],
	/** Deletes a single `Parameter` using a unique key. */
	deleteParameter?:ModelTypes["DeleteParameterPayload"],
	/** Deletes a single `Pary` using its globally unique id. */
	deleteParyByNodeId?:ModelTypes["DeleteParyPayload"],
	/** Deletes a single `Pary` using a unique key. */
	deletePary?:ModelTypes["DeleteParyPayload"],
	/** Deletes a single `ParyNavrh` using its globally unique id. */
	deleteParyNavrhByNodeId?:ModelTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `ParyNavrh` using a unique key. */
	deleteParyNavrh?:ModelTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `Permission` using its globally unique id. */
	deletePermissionByNodeId?:ModelTypes["DeletePermissionPayload"],
	/** Deletes a single `Permission` using a unique key. */
	deletePermission?:ModelTypes["DeletePermissionPayload"],
	/** Deletes a single `PlatbyCategory` using its globally unique id. */
	deletePlatbyCategoryByNodeId?:ModelTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategory` using a unique key. */
	deletePlatbyCategory?:ModelTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using its globally unique id. */
	deletePlatbyCategoryGroupByNodeId?:ModelTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using a unique key. */
	deletePlatbyCategoryGroup?:ModelTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyGroup` using its globally unique id. */
	deletePlatbyGroupByNodeId?:ModelTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroup` using a unique key. */
	deletePlatbyGroup?:ModelTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using its globally unique id. */
	deletePlatbyGroupSkupinaByNodeId?:ModelTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using a unique key. */
	deletePlatbyGroupSkupina?:ModelTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyItem` using its globally unique id. */
	deletePlatbyItemByNodeId?:ModelTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyItem` using a unique key. */
	deletePlatbyItem?:ModelTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyRaw` using its globally unique id. */
	deletePlatbyRawByNodeId?:ModelTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `PlatbyRaw` using a unique key. */
	deletePlatbyRaw?:ModelTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `Rozpi` using its globally unique id. */
	deleteRozpiByNodeId?:ModelTypes["DeleteRozpiPayload"],
	/** Deletes a single `Rozpi` using a unique key. */
	deleteRozpi?:ModelTypes["DeleteRozpiPayload"],
	/** Deletes a single `RozpisItem` using its globally unique id. */
	deleteRozpisItemByNodeId?:ModelTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `RozpisItem` using a unique key. */
	deleteRozpisItem?:ModelTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `Session` using its globally unique id. */
	deleteSessionByNodeId?:ModelTypes["DeleteSessionPayload"],
	/** Deletes a single `Session` using a unique key. */
	deleteSession?:ModelTypes["DeleteSessionPayload"],
	/** Deletes a single `Skupiny` using its globally unique id. */
	deleteSkupinyByNodeId?:ModelTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Skupiny` using a unique key. */
	deleteSkupiny?:ModelTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Upozorneni` using its globally unique id. */
	deleteUpozorneniByNodeId?:ModelTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `Upozorneni` using a unique key. */
	deleteUpozorneni?:ModelTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `UpozorneniSkupiny` using its globally unique id. */
	deleteUpozorneniSkupinyByNodeId?:ModelTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `UpozorneniSkupiny` using a unique key. */
	deleteUpozorneniSkupiny?:ModelTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `User` using its globally unique id. */
	deleteUserByNodeId?:ModelTypes["DeleteUserPayload"],
	/** Deletes a single `User` using a unique key. */
	deleteUser?:ModelTypes["DeleteUserPayload"],
	/** Deletes a single `UsersSkupiny` using its globally unique id. */
	deleteUsersSkupinyByNodeId?:ModelTypes["DeleteUsersSkupinyPayload"],
	/** Deletes a single `UsersSkupiny` using a unique key. */
	deleteUsersSkupiny?:ModelTypes["DeleteUsersSkupinyPayload"],
	/** Deletes a single `Video` using its globally unique id. */
	deleteVideoByNodeId?:ModelTypes["DeleteVideoPayload"],
	/** Deletes a single `Video` using a unique key. */
	deleteVideo?:ModelTypes["DeleteVideoPayload"],
	/** Deletes a single `VideoList` using its globally unique id. */
	deleteVideoListByNodeId?:ModelTypes["DeleteVideoListPayload"],
	/** Deletes a single `VideoList` using a unique key. */
	deleteVideoList?:ModelTypes["DeleteVideoListPayload"],
	/** Deletes a single `VideoSource` using its globally unique id. */
	deleteVideoSourceByNodeId?:ModelTypes["DeleteVideoSourcePayload"],
	/** Deletes a single `VideoSource` using a unique key. */
	deleteVideoSource?:ModelTypes["DeleteVideoSourcePayload"],
	login?:ModelTypes["LoginPayload"],
	logout?:ModelTypes["LogoutPayload"],
	uploadFile:ModelTypes["Upload"]
};
	/** The output of our create `Akce` mutation. */
["CreateAkcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Akce` that was created by this mutation. */
	akce?:ModelTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?:ModelTypes["AkcesEdge"]
};
	/** All input for the create `Akce` mutation. */
["CreateAkceInput"]: GraphQLTypes["CreateAkceInput"];
	/** An input for mutations affecting `Akce` */
["AkceInput"]: GraphQLTypes["AkceInput"];
	/** The output of our create `AkceItem` mutation. */
["CreateAkceItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AkceItem` that was created by this mutation. */
	akceItem?:ModelTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ModelTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ModelTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?:ModelTypes["AkceItemsEdge"]
};
	/** All input for the create `AkceItem` mutation. */
["CreateAkceItemInput"]: GraphQLTypes["CreateAkceItemInput"];
	/** An input for mutations affecting `AkceItem` */
["AkceItemInput"]: GraphQLTypes["AkceItemInput"];
	/** The output of our create `Aktuality` mutation. */
["CreateAktualityPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Aktuality` that was created by this mutation. */
	aktuality?:ModelTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ModelTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ModelTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?:ModelTypes["AktualitiesEdge"]
};
	/** All input for the create `Aktuality` mutation. */
["CreateAktualityInput"]: GraphQLTypes["CreateAktualityInput"];
	/** An input for mutations affecting `Aktuality` */
["AktualityInput"]: GraphQLTypes["AktualityInput"];
	/** The output of our create `Dokumenty` mutation. */
["CreateDokumentyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Dokumenty` that was created by this mutation. */
	dokumenty?:ModelTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ModelTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?:ModelTypes["DokumentiesEdge"]
};
	/** All input for the create `Dokumenty` mutation. */
["CreateDokumentyInput"]: GraphQLTypes["CreateDokumentyInput"];
	/** An input for mutations affecting `Dokumenty` */
["DokumentyInput"]: GraphQLTypes["DokumentyInput"];
	/** The output of our create `GalerieDir` mutation. */
["CreateGalerieDirPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieDir` that was created by this mutation. */
	galerieDir?:ModelTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?:ModelTypes["GalerieDirsEdge"]
};
	/** All input for the create `GalerieDir` mutation. */
["CreateGalerieDirInput"]: GraphQLTypes["CreateGalerieDirInput"];
	/** An input for mutations affecting `GalerieDir` */
["GalerieDirInput"]: GraphQLTypes["GalerieDirInput"];
	/** The output of our create `GalerieFoto` mutation. */
["CreateGalerieFotoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieFoto` that was created by this mutation. */
	galerieFoto?:ModelTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ModelTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ModelTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?:ModelTypes["GalerieFotosEdge"]
};
	/** All input for the create `GalerieFoto` mutation. */
["CreateGalerieFotoInput"]: GraphQLTypes["CreateGalerieFotoInput"];
	/** An input for mutations affecting `GalerieFoto` */
["GalerieFotoInput"]: GraphQLTypes["GalerieFotoInput"];
	/** The output of our create `Nabidka` mutation. */
["CreateNabidkaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Nabidka` that was created by this mutation. */
	nabidka?:ModelTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?:ModelTypes["NabidkasEdge"]
};
	/** All input for the create `Nabidka` mutation. */
["CreateNabidkaInput"]: GraphQLTypes["CreateNabidkaInput"];
	/** An input for mutations affecting `Nabidka` */
["NabidkaInput"]: GraphQLTypes["NabidkaInput"];
	/** The output of our create `NabidkaItem` mutation. */
["CreateNabidkaItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `NabidkaItem` that was created by this mutation. */
	nabidkaItem?:ModelTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ModelTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ModelTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?:ModelTypes["NabidkaItemsEdge"]
};
	/** All input for the create `NabidkaItem` mutation. */
["CreateNabidkaItemInput"]: GraphQLTypes["CreateNabidkaItemInput"];
	/** An input for mutations affecting `NabidkaItem` */
["NabidkaItemInput"]: GraphQLTypes["NabidkaItemInput"];
	/** The output of our create `Page` mutation. */
["CreatePagePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Page` that was created by this mutation. */
	page?:ModelTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Page`. May be used by Relay 1. */
	pageEdge?:ModelTypes["PagesEdge"]
};
	/** All input for the create `Page` mutation. */
["CreatePageInput"]: GraphQLTypes["CreatePageInput"];
	/** An input for mutations affecting `Page` */
["PageInput"]: GraphQLTypes["PageInput"];
	/** The output of our create `Parameter` mutation. */
["CreateParameterPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Parameter` that was created by this mutation. */
	parameter?:ModelTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?:ModelTypes["ParametersEdge"]
};
	/** All input for the create `Parameter` mutation. */
["CreateParameterInput"]: GraphQLTypes["CreateParameterInput"];
	/** An input for mutations affecting `Parameter` */
["ParameterInput"]: GraphQLTypes["ParameterInput"];
	/** The output of our create `Pary` mutation. */
["CreateParyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Pary` that was created by this mutation. */
	pary?:ModelTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ModelTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?:ModelTypes["PariesEdge"]
};
	/** All input for the create `Pary` mutation. */
["CreateParyInput"]: GraphQLTypes["CreateParyInput"];
	/** An input for mutations affecting `Pary` */
["ParyInput"]: GraphQLTypes["ParyInput"];
	/** The output of our create `ParyNavrh` mutation. */
["CreateParyNavrhPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `ParyNavrh` that was created by this mutation. */
	paryNavrh?:ModelTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ModelTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?:ModelTypes["ParyNavrhsEdge"]
};
	/** All input for the create `ParyNavrh` mutation. */
["CreateParyNavrhInput"]: GraphQLTypes["CreateParyNavrhInput"];
	/** An input for mutations affecting `ParyNavrh` */
["ParyNavrhInput"]: GraphQLTypes["ParyNavrhInput"];
	/** The output of our create `Permission` mutation. */
["CreatePermissionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Permission` that was created by this mutation. */
	permission?:ModelTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?:ModelTypes["PermissionsEdge"]
};
	/** All input for the create `Permission` mutation. */
["CreatePermissionInput"]: GraphQLTypes["CreatePermissionInput"];
	/** An input for mutations affecting `Permission` */
["PermissionInput"]: GraphQLTypes["PermissionInput"];
	/** The output of our create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategory` that was created by this mutation. */
	platbyCategory?:ModelTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?:ModelTypes["PlatbyCategoriesEdge"]
};
	/** All input for the create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryInput"]: GraphQLTypes["CreatePlatbyCategoryInput"];
	/** An input for mutations affecting `PlatbyCategory` */
["PlatbyCategoryInput"]: GraphQLTypes["PlatbyCategoryInput"];
	/** The output of our create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategoryGroup` that was created by this mutation. */
	platbyCategoryGroup?:ModelTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ModelTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?:ModelTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupInput"]: GraphQLTypes["CreatePlatbyCategoryGroupInput"];
	/** An input for mutations affecting `PlatbyCategoryGroup` */
["PlatbyCategoryGroupInput"]: GraphQLTypes["PlatbyCategoryGroupInput"];
	/** The output of our create `PlatbyGroup` mutation. */
["CreatePlatbyGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroup` that was created by this mutation. */
	platbyGroup?:ModelTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?:ModelTypes["PlatbyGroupsEdge"]
};
	/** All input for the create `PlatbyGroup` mutation. */
["CreatePlatbyGroupInput"]: GraphQLTypes["CreatePlatbyGroupInput"];
	/** An input for mutations affecting `PlatbyGroup` */
["PlatbyGroupInput"]: GraphQLTypes["PlatbyGroupInput"];
	/** The output of our create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroupSkupina` that was created by this mutation. */
	platbyGroupSkupina?:ModelTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ModelTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ModelTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?:ModelTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaInput"]: GraphQLTypes["CreatePlatbyGroupSkupinaInput"];
	/** An input for mutations affecting `PlatbyGroupSkupina` */
["PlatbyGroupSkupinaInput"]: GraphQLTypes["PlatbyGroupSkupinaInput"];
	/** The output of our create `PlatbyItem` mutation. */
["CreatePlatbyItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyItem` that was created by this mutation. */
	platbyItem?:ModelTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ModelTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ModelTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?:ModelTypes["PlatbyItemsEdge"]
};
	/** All input for the create `PlatbyItem` mutation. */
["CreatePlatbyItemInput"]: GraphQLTypes["CreatePlatbyItemInput"];
	/** An input for mutations affecting `PlatbyItem` */
["PlatbyItemInput"]: GraphQLTypes["PlatbyItemInput"];
	/** The output of our create `PlatbyRaw` mutation. */
["CreatePlatbyRawPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyRaw` that was created by this mutation. */
	platbyRaw?:ModelTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?:ModelTypes["PlatbyRawsEdge"]
};
	/** All input for the create `PlatbyRaw` mutation. */
["CreatePlatbyRawInput"]: GraphQLTypes["CreatePlatbyRawInput"];
	/** An input for mutations affecting `PlatbyRaw` */
["PlatbyRawInput"]: GraphQLTypes["PlatbyRawInput"];
	/** The output of our create `Rozpi` mutation. */
["CreateRozpiPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Rozpi` that was created by this mutation. */
	rozpi?:ModelTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ModelTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?:ModelTypes["RozpisEdge"]
};
	/** All input for the create `Rozpi` mutation. */
["CreateRozpiInput"]: GraphQLTypes["CreateRozpiInput"];
	/** An input for mutations affecting `Rozpi` */
["RozpiInput"]: GraphQLTypes["RozpiInput"];
	/** The output of our create `RozpisItem` mutation. */
["CreateRozpisItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RozpisItem` that was created by this mutation. */
	rozpisItem?:ModelTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ModelTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ModelTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?:ModelTypes["RozpisItemsEdge"]
};
	/** All input for the create `RozpisItem` mutation. */
["CreateRozpisItemInput"]: GraphQLTypes["CreateRozpisItemInput"];
	/** An input for mutations affecting `RozpisItem` */
["RozpisItemInput"]: GraphQLTypes["RozpisItemInput"];
	/** The output of our create `Session` mutation. */
["CreateSessionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Session` that was created by this mutation. */
	session?:ModelTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ModelTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?:ModelTypes["SessionsEdge"]
};
	/** All input for the create `Session` mutation. */
["CreateSessionInput"]: GraphQLTypes["CreateSessionInput"];
	/** An input for mutations affecting `Session` */
["SessionInput"]: GraphQLTypes["SessionInput"];
	/** The output of our create `Skupiny` mutation. */
["CreateSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Skupiny` that was created by this mutation. */
	skupiny?:ModelTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the create `Skupiny` mutation. */
["CreateSkupinyInput"]: GraphQLTypes["CreateSkupinyInput"];
	/** An input for mutations affecting `Skupiny` */
["SkupinyInput"]: GraphQLTypes["SkupinyInput"];
	/** The output of our create `Upozorneni` mutation. */
["CreateUpozorneniPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Upozorneni` that was created by this mutation. */
	upozorneni?:ModelTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ModelTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?:ModelTypes["UpozornenisEdge"]
};
	/** All input for the create `Upozorneni` mutation. */
["CreateUpozorneniInput"]: GraphQLTypes["CreateUpozorneniInput"];
	/** An input for mutations affecting `Upozorneni` */
["UpozorneniInput"]: GraphQLTypes["UpozorneniInput"];
	/** The output of our create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UpozorneniSkupiny` that was created by this mutation. */
	upozorneniSkupiny?:ModelTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ModelTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?:ModelTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyInput"]: GraphQLTypes["CreateUpozorneniSkupinyInput"];
	/** An input for mutations affecting `UpozorneniSkupiny` */
["UpozorneniSkupinyInput"]: GraphQLTypes["UpozorneniSkupinyInput"];
	/** The output of our create `User` mutation. */
["CreateUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `User` that was created by this mutation. */
	user?:ModelTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ModelTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?:ModelTypes["UsersEdge"]
};
	/** All input for the create `User` mutation. */
["CreateUserInput"]: GraphQLTypes["CreateUserInput"];
	/** An input for mutations affecting `User` */
["UserInput"]: GraphQLTypes["UserInput"];
	/** The output of our create `UsersSkupiny` mutation. */
["CreateUsersSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UsersSkupiny` that was created by this mutation. */
	usersSkupiny?:ModelTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?:ModelTypes["UsersSkupiniesEdge"]
};
	/** All input for the create `UsersSkupiny` mutation. */
["CreateUsersSkupinyInput"]: GraphQLTypes["CreateUsersSkupinyInput"];
	/** An input for mutations affecting `UsersSkupiny` */
["UsersSkupinyInput"]: GraphQLTypes["UsersSkupinyInput"];
	/** The output of our create `Video` mutation. */
["CreateVideoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Video` that was created by this mutation. */
	video?:ModelTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?:ModelTypes["VideosEdge"]
};
	/** All input for the create `Video` mutation. */
["CreateVideoInput"]: GraphQLTypes["CreateVideoInput"];
	/** An input for mutations affecting `Video` */
["VideoInput"]: GraphQLTypes["VideoInput"];
	/** The output of our create `VideoList` mutation. */
["CreateVideoListPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoList` that was created by this mutation. */
	videoList?:ModelTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?:ModelTypes["VideoListsEdge"]
};
	/** All input for the create `VideoList` mutation. */
["CreateVideoListInput"]: GraphQLTypes["CreateVideoListInput"];
	/** An input for mutations affecting `VideoList` */
["VideoListInput"]: GraphQLTypes["VideoListInput"];
	/** The output of our create `VideoSource` mutation. */
["CreateVideoSourcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoSource` that was created by this mutation. */
	videoSource?:ModelTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?:ModelTypes["VideoSourcesEdge"]
};
	/** All input for the create `VideoSource` mutation. */
["CreateVideoSourceInput"]: GraphQLTypes["CreateVideoSourceInput"];
	/** An input for mutations affecting `VideoSource` */
["VideoSourceInput"]: GraphQLTypes["VideoSourceInput"];
	/** The output of our update `Akce` mutation. */
["UpdateAkcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Akce` that was updated by this mutation. */
	akce?:ModelTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?:ModelTypes["AkcesEdge"]
};
	/** All input for the `updateAkceByNodeId` mutation. */
["UpdateAkceByNodeIdInput"]: GraphQLTypes["UpdateAkceByNodeIdInput"];
	/** Represents an update to a `Akce`. Fields that are set will be updated. */
["AkcePatch"]: GraphQLTypes["AkcePatch"];
	/** All input for the `updateAkce` mutation. */
["UpdateAkceInput"]: GraphQLTypes["UpdateAkceInput"];
	/** The output of our update `AkceItem` mutation. */
["UpdateAkceItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AkceItem` that was updated by this mutation. */
	akceItem?:ModelTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ModelTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ModelTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?:ModelTypes["AkceItemsEdge"]
};
	/** All input for the `updateAkceItemByNodeId` mutation. */
["UpdateAkceItemByNodeIdInput"]: GraphQLTypes["UpdateAkceItemByNodeIdInput"];
	/** Represents an update to a `AkceItem`. Fields that are set will be updated. */
["AkceItemPatch"]: GraphQLTypes["AkceItemPatch"];
	/** All input for the `updateAkceItem` mutation. */
["UpdateAkceItemInput"]: GraphQLTypes["UpdateAkceItemInput"];
	/** The output of our update `Aktuality` mutation. */
["UpdateAktualityPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Aktuality` that was updated by this mutation. */
	aktuality?:ModelTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ModelTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ModelTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?:ModelTypes["AktualitiesEdge"]
};
	/** All input for the `updateAktualityByNodeId` mutation. */
["UpdateAktualityByNodeIdInput"]: GraphQLTypes["UpdateAktualityByNodeIdInput"];
	/** Represents an update to a `Aktuality`. Fields that are set will be updated. */
["AktualityPatch"]: GraphQLTypes["AktualityPatch"];
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: GraphQLTypes["UpdateAktualityInput"];
	/** The output of our update `Dokumenty` mutation. */
["UpdateDokumentyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Dokumenty` that was updated by this mutation. */
	dokumenty?:ModelTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ModelTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?:ModelTypes["DokumentiesEdge"]
};
	/** All input for the `updateDokumentyByNodeId` mutation. */
["UpdateDokumentyByNodeIdInput"]: GraphQLTypes["UpdateDokumentyByNodeIdInput"];
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: GraphQLTypes["DokumentyPatch"];
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: GraphQLTypes["UpdateDokumentyInput"];
	/** The output of our update `GalerieDir` mutation. */
["UpdateGalerieDirPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieDir` that was updated by this mutation. */
	galerieDir?:ModelTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?:ModelTypes["GalerieDirsEdge"]
};
	/** All input for the `updateGalerieDirByNodeId` mutation. */
["UpdateGalerieDirByNodeIdInput"]: GraphQLTypes["UpdateGalerieDirByNodeIdInput"];
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: GraphQLTypes["GalerieDirPatch"];
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: GraphQLTypes["UpdateGalerieDirInput"];
	/** The output of our update `GalerieFoto` mutation. */
["UpdateGalerieFotoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieFoto` that was updated by this mutation. */
	galerieFoto?:ModelTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ModelTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ModelTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?:ModelTypes["GalerieFotosEdge"]
};
	/** All input for the `updateGalerieFotoByNodeId` mutation. */
["UpdateGalerieFotoByNodeIdInput"]: GraphQLTypes["UpdateGalerieFotoByNodeIdInput"];
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: GraphQLTypes["GalerieFotoPatch"];
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: GraphQLTypes["UpdateGalerieFotoInput"];
	/** The output of our update `Nabidka` mutation. */
["UpdateNabidkaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Nabidka` that was updated by this mutation. */
	nabidka?:ModelTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?:ModelTypes["NabidkasEdge"]
};
	/** All input for the `updateNabidkaByNodeId` mutation. */
["UpdateNabidkaByNodeIdInput"]: GraphQLTypes["UpdateNabidkaByNodeIdInput"];
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: GraphQLTypes["NabidkaPatch"];
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: GraphQLTypes["UpdateNabidkaInput"];
	/** The output of our update `NabidkaItem` mutation. */
["UpdateNabidkaItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `NabidkaItem` that was updated by this mutation. */
	nabidkaItem?:ModelTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ModelTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ModelTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?:ModelTypes["NabidkaItemsEdge"]
};
	/** All input for the `updateNabidkaItemByNodeId` mutation. */
["UpdateNabidkaItemByNodeIdInput"]: GraphQLTypes["UpdateNabidkaItemByNodeIdInput"];
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: GraphQLTypes["NabidkaItemPatch"];
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: GraphQLTypes["UpdateNabidkaItemInput"];
	/** The output of our update `Page` mutation. */
["UpdatePagePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Page` that was updated by this mutation. */
	page?:ModelTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Page`. May be used by Relay 1. */
	pageEdge?:ModelTypes["PagesEdge"]
};
	/** All input for the `updatePageByNodeId` mutation. */
["UpdatePageByNodeIdInput"]: GraphQLTypes["UpdatePageByNodeIdInput"];
	/** Represents an update to a `Page`. Fields that are set will be updated. */
["PagePatch"]: GraphQLTypes["PagePatch"];
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: GraphQLTypes["UpdatePageInput"];
	/** All input for the `updatePageByUrl` mutation. */
["UpdatePageByUrlInput"]: GraphQLTypes["UpdatePageByUrlInput"];
	/** The output of our update `Parameter` mutation. */
["UpdateParameterPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Parameter` that was updated by this mutation. */
	parameter?:ModelTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?:ModelTypes["ParametersEdge"]
};
	/** All input for the `updateParameterByNodeId` mutation. */
["UpdateParameterByNodeIdInput"]: GraphQLTypes["UpdateParameterByNodeIdInput"];
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: GraphQLTypes["ParameterPatch"];
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: GraphQLTypes["UpdateParameterInput"];
	/** The output of our update `Pary` mutation. */
["UpdateParyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Pary` that was updated by this mutation. */
	pary?:ModelTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ModelTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?:ModelTypes["PariesEdge"]
};
	/** All input for the `updateParyByNodeId` mutation. */
["UpdateParyByNodeIdInput"]: GraphQLTypes["UpdateParyByNodeIdInput"];
	/** Represents an update to a `Pary`. Fields that are set will be updated. */
["ParyPatch"]: GraphQLTypes["ParyPatch"];
	/** All input for the `updatePary` mutation. */
["UpdateParyInput"]: GraphQLTypes["UpdateParyInput"];
	/** The output of our update `ParyNavrh` mutation. */
["UpdateParyNavrhPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `ParyNavrh` that was updated by this mutation. */
	paryNavrh?:ModelTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ModelTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?:ModelTypes["ParyNavrhsEdge"]
};
	/** All input for the `updateParyNavrhByNodeId` mutation. */
["UpdateParyNavrhByNodeIdInput"]: GraphQLTypes["UpdateParyNavrhByNodeIdInput"];
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: GraphQLTypes["ParyNavrhPatch"];
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: GraphQLTypes["UpdateParyNavrhInput"];
	/** The output of our update `Permission` mutation. */
["UpdatePermissionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Permission` that was updated by this mutation. */
	permission?:ModelTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?:ModelTypes["PermissionsEdge"]
};
	/** All input for the `updatePermissionByNodeId` mutation. */
["UpdatePermissionByNodeIdInput"]: GraphQLTypes["UpdatePermissionByNodeIdInput"];
	/** Represents an update to a `Permission`. Fields that are set will be updated. */
["PermissionPatch"]: GraphQLTypes["PermissionPatch"];
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: GraphQLTypes["UpdatePermissionInput"];
	/** The output of our update `PlatbyCategory` mutation. */
["UpdatePlatbyCategoryPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategory` that was updated by this mutation. */
	platbyCategory?:ModelTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?:ModelTypes["PlatbyCategoriesEdge"]
};
	/** All input for the `updatePlatbyCategoryByNodeId` mutation. */
["UpdatePlatbyCategoryByNodeIdInput"]: GraphQLTypes["UpdatePlatbyCategoryByNodeIdInput"];
	/** Represents an update to a `PlatbyCategory`. Fields that are set will be updated. */
["PlatbyCategoryPatch"]: GraphQLTypes["PlatbyCategoryPatch"];
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: GraphQLTypes["UpdatePlatbyCategoryInput"];
	/** The output of our update `PlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategoryGroup` that was updated by this mutation. */
	platbyCategoryGroup?:ModelTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ModelTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?:ModelTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the `updatePlatbyCategoryGroupByNodeId` mutation. */
["UpdatePlatbyCategoryGroupByNodeIdInput"]: GraphQLTypes["UpdatePlatbyCategoryGroupByNodeIdInput"];
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: GraphQLTypes["PlatbyCategoryGroupPatch"];
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: GraphQLTypes["UpdatePlatbyCategoryGroupInput"];
	/** The output of our update `PlatbyGroup` mutation. */
["UpdatePlatbyGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroup` that was updated by this mutation. */
	platbyGroup?:ModelTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?:ModelTypes["PlatbyGroupsEdge"]
};
	/** All input for the `updatePlatbyGroupByNodeId` mutation. */
["UpdatePlatbyGroupByNodeIdInput"]: GraphQLTypes["UpdatePlatbyGroupByNodeIdInput"];
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: GraphQLTypes["PlatbyGroupPatch"];
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: GraphQLTypes["UpdatePlatbyGroupInput"];
	/** The output of our update `PlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroupSkupina` that was updated by this mutation. */
	platbyGroupSkupina?:ModelTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ModelTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ModelTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?:ModelTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the `updatePlatbyGroupSkupinaByNodeId` mutation. */
["UpdatePlatbyGroupSkupinaByNodeIdInput"]: GraphQLTypes["UpdatePlatbyGroupSkupinaByNodeIdInput"];
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: GraphQLTypes["PlatbyGroupSkupinaPatch"];
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: GraphQLTypes["UpdatePlatbyGroupSkupinaInput"];
	/** The output of our update `PlatbyItem` mutation. */
["UpdatePlatbyItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyItem` that was updated by this mutation. */
	platbyItem?:ModelTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ModelTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ModelTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?:ModelTypes["PlatbyItemsEdge"]
};
	/** All input for the `updatePlatbyItemByNodeId` mutation. */
["UpdatePlatbyItemByNodeIdInput"]: GraphQLTypes["UpdatePlatbyItemByNodeIdInput"];
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: GraphQLTypes["PlatbyItemPatch"];
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: GraphQLTypes["UpdatePlatbyItemInput"];
	/** The output of our update `PlatbyRaw` mutation. */
["UpdatePlatbyRawPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyRaw` that was updated by this mutation. */
	platbyRaw?:ModelTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?:ModelTypes["PlatbyRawsEdge"]
};
	/** All input for the `updatePlatbyRawByNodeId` mutation. */
["UpdatePlatbyRawByNodeIdInput"]: GraphQLTypes["UpdatePlatbyRawByNodeIdInput"];
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: GraphQLTypes["PlatbyRawPatch"];
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: GraphQLTypes["UpdatePlatbyRawInput"];
	/** The output of our update `Rozpi` mutation. */
["UpdateRozpiPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Rozpi` that was updated by this mutation. */
	rozpi?:ModelTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ModelTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?:ModelTypes["RozpisEdge"]
};
	/** All input for the `updateRozpiByNodeId` mutation. */
["UpdateRozpiByNodeIdInput"]: GraphQLTypes["UpdateRozpiByNodeIdInput"];
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: GraphQLTypes["RozpiPatch"];
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: GraphQLTypes["UpdateRozpiInput"];
	/** The output of our update `RozpisItem` mutation. */
["UpdateRozpisItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RozpisItem` that was updated by this mutation. */
	rozpisItem?:ModelTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ModelTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ModelTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?:ModelTypes["RozpisItemsEdge"]
};
	/** All input for the `updateRozpisItemByNodeId` mutation. */
["UpdateRozpisItemByNodeIdInput"]: GraphQLTypes["UpdateRozpisItemByNodeIdInput"];
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: GraphQLTypes["RozpisItemPatch"];
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: GraphQLTypes["UpdateRozpisItemInput"];
	/** The output of our update `Session` mutation. */
["UpdateSessionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Session` that was updated by this mutation. */
	session?:ModelTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ModelTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?:ModelTypes["SessionsEdge"]
};
	/** All input for the `updateSessionByNodeId` mutation. */
["UpdateSessionByNodeIdInput"]: GraphQLTypes["UpdateSessionByNodeIdInput"];
	/** Represents an update to a `Session`. Fields that are set will be updated. */
["SessionPatch"]: GraphQLTypes["SessionPatch"];
	/** All input for the `updateSession` mutation. */
["UpdateSessionInput"]: GraphQLTypes["UpdateSessionInput"];
	/** The output of our update `Skupiny` mutation. */
["UpdateSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Skupiny` that was updated by this mutation. */
	skupiny?:ModelTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the `updateSkupinyByNodeId` mutation. */
["UpdateSkupinyByNodeIdInput"]: GraphQLTypes["UpdateSkupinyByNodeIdInput"];
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: GraphQLTypes["SkupinyPatch"];
	/** All input for the `updateSkupiny` mutation. */
["UpdateSkupinyInput"]: GraphQLTypes["UpdateSkupinyInput"];
	/** The output of our update `Upozorneni` mutation. */
["UpdateUpozorneniPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Upozorneni` that was updated by this mutation. */
	upozorneni?:ModelTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ModelTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?:ModelTypes["UpozornenisEdge"]
};
	/** All input for the `updateUpozorneniByNodeId` mutation. */
["UpdateUpozorneniByNodeIdInput"]: GraphQLTypes["UpdateUpozorneniByNodeIdInput"];
	/** Represents an update to a `Upozorneni`. Fields that are set will be updated. */
["UpozorneniPatch"]: GraphQLTypes["UpozorneniPatch"];
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: GraphQLTypes["UpdateUpozorneniInput"];
	/** The output of our update `UpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UpozorneniSkupiny` that was updated by this mutation. */
	upozorneniSkupiny?:ModelTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ModelTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?:ModelTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the `updateUpozorneniSkupinyByNodeId` mutation. */
["UpdateUpozorneniSkupinyByNodeIdInput"]: GraphQLTypes["UpdateUpozorneniSkupinyByNodeIdInput"];
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: GraphQLTypes["UpozorneniSkupinyPatch"];
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: GraphQLTypes["UpdateUpozorneniSkupinyInput"];
	/** The output of our update `User` mutation. */
["UpdateUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `User` that was updated by this mutation. */
	user?:ModelTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ModelTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?:ModelTypes["UsersEdge"]
};
	/** All input for the `updateUserByNodeId` mutation. */
["UpdateUserByNodeIdInput"]: GraphQLTypes["UpdateUserByNodeIdInput"];
	/** Represents an update to a `User`. Fields that are set will be updated. */
["UserPatch"]: GraphQLTypes["UserPatch"];
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: GraphQLTypes["UpdateUserInput"];
	/** The output of our update `UsersSkupiny` mutation. */
["UpdateUsersSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UsersSkupiny` that was updated by this mutation. */
	usersSkupiny?:ModelTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?:ModelTypes["UsersSkupiniesEdge"]
};
	/** All input for the `updateUsersSkupinyByNodeId` mutation. */
["UpdateUsersSkupinyByNodeIdInput"]: GraphQLTypes["UpdateUsersSkupinyByNodeIdInput"];
	/** Represents an update to a `UsersSkupiny`. Fields that are set will be updated. */
["UsersSkupinyPatch"]: GraphQLTypes["UsersSkupinyPatch"];
	/** All input for the `updateUsersSkupiny` mutation. */
["UpdateUsersSkupinyInput"]: GraphQLTypes["UpdateUsersSkupinyInput"];
	/** The output of our update `Video` mutation. */
["UpdateVideoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Video` that was updated by this mutation. */
	video?:ModelTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?:ModelTypes["VideosEdge"]
};
	/** All input for the `updateVideoByNodeId` mutation. */
["UpdateVideoByNodeIdInput"]: GraphQLTypes["UpdateVideoByNodeIdInput"];
	/** Represents an update to a `Video`. Fields that are set will be updated. */
["VideoPatch"]: GraphQLTypes["VideoPatch"];
	/** All input for the `updateVideo` mutation. */
["UpdateVideoInput"]: GraphQLTypes["UpdateVideoInput"];
	/** The output of our update `VideoList` mutation. */
["UpdateVideoListPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoList` that was updated by this mutation. */
	videoList?:ModelTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?:ModelTypes["VideoListsEdge"]
};
	/** All input for the `updateVideoListByNodeId` mutation. */
["UpdateVideoListByNodeIdInput"]: GraphQLTypes["UpdateVideoListByNodeIdInput"];
	/** Represents an update to a `VideoList`. Fields that are set will be updated. */
["VideoListPatch"]: GraphQLTypes["VideoListPatch"];
	/** All input for the `updateVideoList` mutation. */
["UpdateVideoListInput"]: GraphQLTypes["UpdateVideoListInput"];
	/** The output of our update `VideoSource` mutation. */
["UpdateVideoSourcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoSource` that was updated by this mutation. */
	videoSource?:ModelTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?:ModelTypes["VideoSourcesEdge"]
};
	/** All input for the `updateVideoSourceByNodeId` mutation. */
["UpdateVideoSourceByNodeIdInput"]: GraphQLTypes["UpdateVideoSourceByNodeIdInput"];
	/** Represents an update to a `VideoSource`. Fields that are set will be updated. */
["VideoSourcePatch"]: GraphQLTypes["VideoSourcePatch"];
	/** All input for the `updateVideoSource` mutation. */
["UpdateVideoSourceInput"]: GraphQLTypes["UpdateVideoSourceInput"];
	/** The output of our delete `Akce` mutation. */
["DeleteAkcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Akce` that was deleted by this mutation. */
	akce?:ModelTypes["Akce"],
	deletedAkceNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?:ModelTypes["AkcesEdge"]
};
	/** All input for the `deleteAkceByNodeId` mutation. */
["DeleteAkceByNodeIdInput"]: GraphQLTypes["DeleteAkceByNodeIdInput"];
	/** All input for the `deleteAkce` mutation. */
["DeleteAkceInput"]: GraphQLTypes["DeleteAkceInput"];
	/** The output of our delete `AkceItem` mutation. */
["DeleteAkceItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AkceItem` that was deleted by this mutation. */
	akceItem?:ModelTypes["AkceItem"],
	deletedAkceItemNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ModelTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ModelTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?:ModelTypes["AkceItemsEdge"]
};
	/** All input for the `deleteAkceItemByNodeId` mutation. */
["DeleteAkceItemByNodeIdInput"]: GraphQLTypes["DeleteAkceItemByNodeIdInput"];
	/** All input for the `deleteAkceItem` mutation. */
["DeleteAkceItemInput"]: GraphQLTypes["DeleteAkceItemInput"];
	/** The output of our delete `Aktuality` mutation. */
["DeleteAktualityPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Aktuality` that was deleted by this mutation. */
	aktuality?:ModelTypes["Aktuality"],
	deletedAktualityNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ModelTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ModelTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?:ModelTypes["AktualitiesEdge"]
};
	/** All input for the `deleteAktualityByNodeId` mutation. */
["DeleteAktualityByNodeIdInput"]: GraphQLTypes["DeleteAktualityByNodeIdInput"];
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: GraphQLTypes["DeleteAktualityInput"];
	/** The output of our delete `Dokumenty` mutation. */
["DeleteDokumentyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Dokumenty` that was deleted by this mutation. */
	dokumenty?:ModelTypes["Dokumenty"],
	deletedDokumentyNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?:ModelTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?:ModelTypes["DokumentiesEdge"]
};
	/** All input for the `deleteDokumentyByNodeId` mutation. */
["DeleteDokumentyByNodeIdInput"]: GraphQLTypes["DeleteDokumentyByNodeIdInput"];
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: GraphQLTypes["DeleteDokumentyInput"];
	/** The output of our delete `GalerieDir` mutation. */
["DeleteGalerieDirPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieDir` that was deleted by this mutation. */
	galerieDir?:ModelTypes["GalerieDir"],
	deletedGalerieDirNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?:ModelTypes["GalerieDirsEdge"]
};
	/** All input for the `deleteGalerieDirByNodeId` mutation. */
["DeleteGalerieDirByNodeIdInput"]: GraphQLTypes["DeleteGalerieDirByNodeIdInput"];
	/** All input for the `deleteGalerieDir` mutation. */
["DeleteGalerieDirInput"]: GraphQLTypes["DeleteGalerieDirInput"];
	/** The output of our delete `GalerieFoto` mutation. */
["DeleteGalerieFotoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `GalerieFoto` that was deleted by this mutation. */
	galerieFoto?:ModelTypes["GalerieFoto"],
	deletedGalerieFotoNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ModelTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ModelTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?:ModelTypes["GalerieFotosEdge"]
};
	/** All input for the `deleteGalerieFotoByNodeId` mutation. */
["DeleteGalerieFotoByNodeIdInput"]: GraphQLTypes["DeleteGalerieFotoByNodeIdInput"];
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: GraphQLTypes["DeleteGalerieFotoInput"];
	/** The output of our delete `Nabidka` mutation. */
["DeleteNabidkaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Nabidka` that was deleted by this mutation. */
	nabidka?:ModelTypes["Nabidka"],
	deletedNabidkaNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?:ModelTypes["NabidkasEdge"]
};
	/** All input for the `deleteNabidkaByNodeId` mutation. */
["DeleteNabidkaByNodeIdInput"]: GraphQLTypes["DeleteNabidkaByNodeIdInput"];
	/** All input for the `deleteNabidka` mutation. */
["DeleteNabidkaInput"]: GraphQLTypes["DeleteNabidkaInput"];
	/** The output of our delete `NabidkaItem` mutation. */
["DeleteNabidkaItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `NabidkaItem` that was deleted by this mutation. */
	nabidkaItem?:ModelTypes["NabidkaItem"],
	deletedNabidkaItemNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ModelTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ModelTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?:ModelTypes["NabidkaItemsEdge"]
};
	/** All input for the `deleteNabidkaItemByNodeId` mutation. */
["DeleteNabidkaItemByNodeIdInput"]: GraphQLTypes["DeleteNabidkaItemByNodeIdInput"];
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: GraphQLTypes["DeleteNabidkaItemInput"];
	/** The output of our delete `Parameter` mutation. */
["DeleteParameterPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Parameter` that was deleted by this mutation. */
	parameter?:ModelTypes["Parameter"],
	deletedParameterNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?:ModelTypes["ParametersEdge"]
};
	/** All input for the `deleteParameterByNodeId` mutation. */
["DeleteParameterByNodeIdInput"]: GraphQLTypes["DeleteParameterByNodeIdInput"];
	/** All input for the `deleteParameter` mutation. */
["DeleteParameterInput"]: GraphQLTypes["DeleteParameterInput"];
	/** The output of our delete `Pary` mutation. */
["DeleteParyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Pary` that was deleted by this mutation. */
	pary?:ModelTypes["Pary"],
	deletedParyNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ModelTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?:ModelTypes["PariesEdge"]
};
	/** All input for the `deleteParyByNodeId` mutation. */
["DeleteParyByNodeIdInput"]: GraphQLTypes["DeleteParyByNodeIdInput"];
	/** All input for the `deletePary` mutation. */
["DeleteParyInput"]: GraphQLTypes["DeleteParyInput"];
	/** The output of our delete `ParyNavrh` mutation. */
["DeleteParyNavrhPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `ParyNavrh` that was deleted by this mutation. */
	paryNavrh?:ModelTypes["ParyNavrh"],
	deletedParyNavrhNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?:ModelTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?:ModelTypes["ParyNavrhsEdge"]
};
	/** All input for the `deleteParyNavrhByNodeId` mutation. */
["DeleteParyNavrhByNodeIdInput"]: GraphQLTypes["DeleteParyNavrhByNodeIdInput"];
	/** All input for the `deleteParyNavrh` mutation. */
["DeleteParyNavrhInput"]: GraphQLTypes["DeleteParyNavrhInput"];
	/** The output of our delete `Permission` mutation. */
["DeletePermissionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Permission` that was deleted by this mutation. */
	permission?:ModelTypes["Permission"],
	deletedPermissionNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?:ModelTypes["PermissionsEdge"]
};
	/** All input for the `deletePermissionByNodeId` mutation. */
["DeletePermissionByNodeIdInput"]: GraphQLTypes["DeletePermissionByNodeIdInput"];
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: GraphQLTypes["DeletePermissionInput"];
	/** The output of our delete `PlatbyCategory` mutation. */
["DeletePlatbyCategoryPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategory` that was deleted by this mutation. */
	platbyCategory?:ModelTypes["PlatbyCategory"],
	deletedPlatbyCategoryNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?:ModelTypes["PlatbyCategoriesEdge"]
};
	/** All input for the `deletePlatbyCategoryByNodeId` mutation. */
["DeletePlatbyCategoryByNodeIdInput"]: GraphQLTypes["DeletePlatbyCategoryByNodeIdInput"];
	/** All input for the `deletePlatbyCategory` mutation. */
["DeletePlatbyCategoryInput"]: GraphQLTypes["DeletePlatbyCategoryInput"];
	/** The output of our delete `PlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyCategoryGroup` that was deleted by this mutation. */
	platbyCategoryGroup?:ModelTypes["PlatbyCategoryGroup"],
	deletedPlatbyCategoryGroupNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ModelTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?:ModelTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the `deletePlatbyCategoryGroupByNodeId` mutation. */
["DeletePlatbyCategoryGroupByNodeIdInput"]: GraphQLTypes["DeletePlatbyCategoryGroupByNodeIdInput"];
	/** All input for the `deletePlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupInput"]: GraphQLTypes["DeletePlatbyCategoryGroupInput"];
	/** The output of our delete `PlatbyGroup` mutation. */
["DeletePlatbyGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroup` that was deleted by this mutation. */
	platbyGroup?:ModelTypes["PlatbyGroup"],
	deletedPlatbyGroupNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?:ModelTypes["PlatbyGroupsEdge"]
};
	/** All input for the `deletePlatbyGroupByNodeId` mutation. */
["DeletePlatbyGroupByNodeIdInput"]: GraphQLTypes["DeletePlatbyGroupByNodeIdInput"];
	/** All input for the `deletePlatbyGroup` mutation. */
["DeletePlatbyGroupInput"]: GraphQLTypes["DeletePlatbyGroupInput"];
	/** The output of our delete `PlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyGroupSkupina` that was deleted by this mutation. */
	platbyGroupSkupina?:ModelTypes["PlatbyGroupSkupina"],
	deletedPlatbyGroupSkupinaNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ModelTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ModelTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?:ModelTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the `deletePlatbyGroupSkupinaByNodeId` mutation. */
["DeletePlatbyGroupSkupinaByNodeIdInput"]: GraphQLTypes["DeletePlatbyGroupSkupinaByNodeIdInput"];
	/** All input for the `deletePlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaInput"]: GraphQLTypes["DeletePlatbyGroupSkupinaInput"];
	/** The output of our delete `PlatbyItem` mutation. */
["DeletePlatbyItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyItem` that was deleted by this mutation. */
	platbyItem?:ModelTypes["PlatbyItem"],
	deletedPlatbyItemNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ModelTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ModelTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?:ModelTypes["PlatbyItemsEdge"]
};
	/** All input for the `deletePlatbyItemByNodeId` mutation. */
["DeletePlatbyItemByNodeIdInput"]: GraphQLTypes["DeletePlatbyItemByNodeIdInput"];
	/** All input for the `deletePlatbyItem` mutation. */
["DeletePlatbyItemInput"]: GraphQLTypes["DeletePlatbyItemInput"];
	/** The output of our delete `PlatbyRaw` mutation. */
["DeletePlatbyRawPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `PlatbyRaw` that was deleted by this mutation. */
	platbyRaw?:ModelTypes["PlatbyRaw"],
	deletedPlatbyRawNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?:ModelTypes["PlatbyRawsEdge"]
};
	/** All input for the `deletePlatbyRawByNodeId` mutation. */
["DeletePlatbyRawByNodeIdInput"]: GraphQLTypes["DeletePlatbyRawByNodeIdInput"];
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: GraphQLTypes["DeletePlatbyRawInput"];
	/** The output of our delete `Rozpi` mutation. */
["DeleteRozpiPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Rozpi` that was deleted by this mutation. */
	rozpi?:ModelTypes["Rozpi"],
	deletedRozpiNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?:ModelTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?:ModelTypes["RozpisEdge"]
};
	/** All input for the `deleteRozpiByNodeId` mutation. */
["DeleteRozpiByNodeIdInput"]: GraphQLTypes["DeleteRozpiByNodeIdInput"];
	/** All input for the `deleteRozpi` mutation. */
["DeleteRozpiInput"]: GraphQLTypes["DeleteRozpiInput"];
	/** The output of our delete `RozpisItem` mutation. */
["DeleteRozpisItemPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RozpisItem` that was deleted by this mutation. */
	rozpisItem?:ModelTypes["RozpisItem"],
	deletedRozpisItemNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ModelTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ModelTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?:ModelTypes["RozpisItemsEdge"]
};
	/** All input for the `deleteRozpisItemByNodeId` mutation. */
["DeleteRozpisItemByNodeIdInput"]: GraphQLTypes["DeleteRozpisItemByNodeIdInput"];
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: GraphQLTypes["DeleteRozpisItemInput"];
	/** The output of our delete `Session` mutation. */
["DeleteSessionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Session` that was deleted by this mutation. */
	session?:ModelTypes["Session"],
	deletedSessionNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?:ModelTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?:ModelTypes["SessionsEdge"]
};
	/** All input for the `deleteSessionByNodeId` mutation. */
["DeleteSessionByNodeIdInput"]: GraphQLTypes["DeleteSessionByNodeIdInput"];
	/** All input for the `deleteSession` mutation. */
["DeleteSessionInput"]: GraphQLTypes["DeleteSessionInput"];
	/** The output of our delete `Skupiny` mutation. */
["DeleteSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Skupiny` that was deleted by this mutation. */
	skupiny?:ModelTypes["Skupiny"],
	deletedSkupinyNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the `deleteSkupinyByNodeId` mutation. */
["DeleteSkupinyByNodeIdInput"]: GraphQLTypes["DeleteSkupinyByNodeIdInput"];
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: GraphQLTypes["DeleteSkupinyInput"];
	/** The output of our delete `Upozorneni` mutation. */
["DeleteUpozorneniPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Upozorneni` that was deleted by this mutation. */
	upozorneni?:ModelTypes["Upozorneni"],
	deletedUpozorneniNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?:ModelTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?:ModelTypes["UpozornenisEdge"]
};
	/** All input for the `deleteUpozorneniByNodeId` mutation. */
["DeleteUpozorneniByNodeIdInput"]: GraphQLTypes["DeleteUpozorneniByNodeIdInput"];
	/** All input for the `deleteUpozorneni` mutation. */
["DeleteUpozorneniInput"]: GraphQLTypes["DeleteUpozorneniInput"];
	/** The output of our delete `UpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UpozorneniSkupiny` that was deleted by this mutation. */
	upozorneniSkupiny?:ModelTypes["UpozorneniSkupiny"],
	deletedUpozorneniSkupinyNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ModelTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?:ModelTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the `deleteUpozorneniSkupinyByNodeId` mutation. */
["DeleteUpozorneniSkupinyByNodeIdInput"]: GraphQLTypes["DeleteUpozorneniSkupinyByNodeIdInput"];
	/** All input for the `deleteUpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyInput"]: GraphQLTypes["DeleteUpozorneniSkupinyInput"];
	/** The output of our delete `User` mutation. */
["DeleteUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `User` that was deleted by this mutation. */
	user?:ModelTypes["User"],
	deletedUserNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ModelTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ModelTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?:ModelTypes["UsersEdge"]
};
	/** All input for the `deleteUserByNodeId` mutation. */
["DeleteUserByNodeIdInput"]: GraphQLTypes["DeleteUserByNodeIdInput"];
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: GraphQLTypes["DeleteUserInput"];
	/** The output of our delete `UsersSkupiny` mutation. */
["DeleteUsersSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `UsersSkupiny` that was deleted by this mutation. */
	usersSkupiny?:ModelTypes["UsersSkupiny"],
	deletedUsersSkupinyNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?:ModelTypes["UsersSkupiniesEdge"]
};
	/** All input for the `deleteUsersSkupinyByNodeId` mutation. */
["DeleteUsersSkupinyByNodeIdInput"]: GraphQLTypes["DeleteUsersSkupinyByNodeIdInput"];
	/** All input for the `deleteUsersSkupiny` mutation. */
["DeleteUsersSkupinyInput"]: GraphQLTypes["DeleteUsersSkupinyInput"];
	/** The output of our delete `Video` mutation. */
["DeleteVideoPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Video` that was deleted by this mutation. */
	video?:ModelTypes["Video"],
	deletedVideoNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?:ModelTypes["VideosEdge"]
};
	/** All input for the `deleteVideoByNodeId` mutation. */
["DeleteVideoByNodeIdInput"]: GraphQLTypes["DeleteVideoByNodeIdInput"];
	/** All input for the `deleteVideo` mutation. */
["DeleteVideoInput"]: GraphQLTypes["DeleteVideoInput"];
	/** The output of our delete `VideoList` mutation. */
["DeleteVideoListPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoList` that was deleted by this mutation. */
	videoList?:ModelTypes["VideoList"],
	deletedVideoListNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?:ModelTypes["VideoListsEdge"]
};
	/** All input for the `deleteVideoListByNodeId` mutation. */
["DeleteVideoListByNodeIdInput"]: GraphQLTypes["DeleteVideoListByNodeIdInput"];
	/** All input for the `deleteVideoList` mutation. */
["DeleteVideoListInput"]: GraphQLTypes["DeleteVideoListInput"];
	/** The output of our delete `VideoSource` mutation. */
["DeleteVideoSourcePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `VideoSource` that was deleted by this mutation. */
	videoSource?:ModelTypes["VideoSource"],
	deletedVideoSourceNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?:ModelTypes["VideoSourcesEdge"]
};
	/** All input for the `deleteVideoSourceByNodeId` mutation. */
["DeleteVideoSourceByNodeIdInput"]: GraphQLTypes["DeleteVideoSourceByNodeIdInput"];
	/** All input for the `deleteVideoSource` mutation. */
["DeleteVideoSourceInput"]: GraphQLTypes["DeleteVideoSourceInput"];
	/** The output of our `login` mutation. */
["LoginPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	result?:ModelTypes["LoginRecord"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** The return type of our `login` mutation. */
["LoginRecord"]: {
		couple?:ModelTypes["Pary"],
	sess?:ModelTypes["Session"],
	usr?:ModelTypes["User"]
};
	/** All input for the `login` mutation. */
["LoginInput"]: GraphQLTypes["LoginInput"];
	/** The output of our `logout` mutation. */
["LogoutPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `logout` mutation. */
["LogoutInput"]: GraphQLTypes["LogoutInput"];
	["Upload"]: {
		uploadUrl:string
};
	["UploadInput"]: GraphQLTypes["UploadInput"]
    }

export type GraphQLTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: {
	__typename: "Query",
	/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query: GraphQLTypes["Query"],
	/** The root query type must be a `Node` to work well with Relay 1 mutations. This just resolves to `query`. */
	nodeId: string,
	/** Fetches an object given its globally unique `ID`. */
	node?: GraphQLTypes["Node"],
	/** Reads and enables pagination through a set of `Akce`. */
	akces?: GraphQLTypes["AkcesConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItems?: GraphQLTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualities?: GraphQLTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumenties?: GraphQLTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieDir`. */
	galerieDirs?: GraphQLTypes["GalerieDirsConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotos?: GraphQLTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `Member`. */
	members?: GraphQLTypes["MembersConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkas?: GraphQLTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItems?: GraphQLTypes["NabidkaItemsConnection"],
	/** Reads and enables pagination through a set of `Page`. */
	pages?: GraphQLTypes["PagesConnection"],
	/** Reads and enables pagination through a set of `PageRevision`. */
	pageRevisions?: GraphQLTypes["PageRevisionsConnection"],
	/** Reads and enables pagination through a set of `Parameter`. */
	parameters?: GraphQLTypes["ParametersConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	paries?: GraphQLTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhs?: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Permission`. */
	permissions?: GraphQLTypes["PermissionsConnection"],
	/** Reads and enables pagination through a set of `PlatbyCategory`. */
	platbyCategories?: GraphQLTypes["PlatbyCategoriesConnection"],
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroups?: GraphQLTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroup`. */
	platbyGroups?: GraphQLTypes["PlatbyGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinas?: GraphQLTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItems?: GraphQLTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `PlatbyRaw`. */
	platbyRaws?: GraphQLTypes["PlatbyRawsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpis?: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItems?: GraphQLTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessions?: GraphQLTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupinies?: GraphQLTypes["SkupiniesConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenis?: GraphQLTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupinies?: GraphQLTypes["UpozorneniSkupiniesConnection"],
	/** Reads and enables pagination through a set of `User`. */
	users?: GraphQLTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `UsersSkupiny`. */
	usersSkupinies?: GraphQLTypes["UsersSkupiniesConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	videos?: GraphQLTypes["VideosConnection"],
	/** Reads and enables pagination through a set of `VideoList`. */
	videoLists?: GraphQLTypes["VideoListsConnection"],
	/** Reads and enables pagination through a set of `VideoSource`. */
	videoSources?: GraphQLTypes["VideoSourcesConnection"],
	akce?: GraphQLTypes["Akce"],
	akceItem?: GraphQLTypes["AkceItem"],
	aktuality?: GraphQLTypes["Aktuality"],
	dokumenty?: GraphQLTypes["Dokumenty"],
	galerieDir?: GraphQLTypes["GalerieDir"],
	galerieFoto?: GraphQLTypes["GalerieFoto"],
	nabidka?: GraphQLTypes["Nabidka"],
	nabidkaItem?: GraphQLTypes["NabidkaItem"],
	page?: GraphQLTypes["Page"],
	pageByUrl?: GraphQLTypes["Page"],
	pageRevision?: GraphQLTypes["PageRevision"],
	parameter?: GraphQLTypes["Parameter"],
	pary?: GraphQLTypes["Pary"],
	paryNavrh?: GraphQLTypes["ParyNavrh"],
	permission?: GraphQLTypes["Permission"],
	platbyCategory?: GraphQLTypes["PlatbyCategory"],
	platbyCategoryGroup?: GraphQLTypes["PlatbyCategoryGroup"],
	platbyGroup?: GraphQLTypes["PlatbyGroup"],
	platbyGroupSkupina?: GraphQLTypes["PlatbyGroupSkupina"],
	platbyItem?: GraphQLTypes["PlatbyItem"],
	platbyRaw?: GraphQLTypes["PlatbyRaw"],
	rozpi?: GraphQLTypes["Rozpi"],
	rozpisItem?: GraphQLTypes["RozpisItem"],
	session?: GraphQLTypes["Session"],
	skupiny?: GraphQLTypes["Skupiny"],
	upozorneni?: GraphQLTypes["Upozorneni"],
	upozorneniSkupiny?: GraphQLTypes["UpozorneniSkupiny"],
	user?: GraphQLTypes["User"],
	usersSkupiny?: GraphQLTypes["UsersSkupiny"],
	video?: GraphQLTypes["Video"],
	videoList?: GraphQLTypes["VideoList"],
	videoSource?: GraphQLTypes["VideoSource"],
	currentCoupleIds?: GraphQLTypes["CurrentCoupleIdsConnection"],
	currentSessionId?: string,
	currentUserId?: GraphQLTypes["BigInt"],
	getCurrentUser?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	reservationsForRange?: GraphQLTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	schedulesForRange?: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	titleVideos?: GraphQLTypes["VideosConnection"],
	/** Reads a single `Akce` using its globally unique `ID`. */
	akceByNodeId?: GraphQLTypes["Akce"],
	/** Reads a single `AkceItem` using its globally unique `ID`. */
	akceItemByNodeId?: GraphQLTypes["AkceItem"],
	/** Reads a single `Aktuality` using its globally unique `ID`. */
	aktualityByNodeId?: GraphQLTypes["Aktuality"],
	/** Reads a single `Dokumenty` using its globally unique `ID`. */
	dokumentyByNodeId?: GraphQLTypes["Dokumenty"],
	/** Reads a single `GalerieDir` using its globally unique `ID`. */
	galerieDirByNodeId?: GraphQLTypes["GalerieDir"],
	/** Reads a single `GalerieFoto` using its globally unique `ID`. */
	galerieFotoByNodeId?: GraphQLTypes["GalerieFoto"],
	/** Reads a single `Nabidka` using its globally unique `ID`. */
	nabidkaByNodeId?: GraphQLTypes["Nabidka"],
	/** Reads a single `NabidkaItem` using its globally unique `ID`. */
	nabidkaItemByNodeId?: GraphQLTypes["NabidkaItem"],
	/** Reads a single `Page` using its globally unique `ID`. */
	pageByNodeId?: GraphQLTypes["Page"],
	/** Reads a single `PageRevision` using its globally unique `ID`. */
	pageRevisionByNodeId?: GraphQLTypes["PageRevision"],
	/** Reads a single `Parameter` using its globally unique `ID`. */
	parameterByNodeId?: GraphQLTypes["Parameter"],
	/** Reads a single `Pary` using its globally unique `ID`. */
	paryByNodeId?: GraphQLTypes["Pary"],
	/** Reads a single `ParyNavrh` using its globally unique `ID`. */
	paryNavrhByNodeId?: GraphQLTypes["ParyNavrh"],
	/** Reads a single `Permission` using its globally unique `ID`. */
	permissionByNodeId?: GraphQLTypes["Permission"],
	/** Reads a single `PlatbyCategory` using its globally unique `ID`. */
	platbyCategoryByNodeId?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyCategoryGroup` using its globally unique `ID`. */
	platbyCategoryGroupByNodeId?: GraphQLTypes["PlatbyCategoryGroup"],
	/** Reads a single `PlatbyGroup` using its globally unique `ID`. */
	platbyGroupByNodeId?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyGroupSkupina` using its globally unique `ID`. */
	platbyGroupSkupinaByNodeId?: GraphQLTypes["PlatbyGroupSkupina"],
	/** Reads a single `PlatbyItem` using its globally unique `ID`. */
	platbyItemByNodeId?: GraphQLTypes["PlatbyItem"],
	/** Reads a single `PlatbyRaw` using its globally unique `ID`. */
	platbyRawByNodeId?: GraphQLTypes["PlatbyRaw"],
	/** Reads a single `Rozpi` using its globally unique `ID`. */
	rozpiByNodeId?: GraphQLTypes["Rozpi"],
	/** Reads a single `RozpisItem` using its globally unique `ID`. */
	rozpisItemByNodeId?: GraphQLTypes["RozpisItem"],
	/** Reads a single `Session` using its globally unique `ID`. */
	sessionByNodeId?: GraphQLTypes["Session"],
	/** Reads a single `Skupiny` using its globally unique `ID`. */
	skupinyByNodeId?: GraphQLTypes["Skupiny"],
	/** Reads a single `Upozorneni` using its globally unique `ID`. */
	upozorneniByNodeId?: GraphQLTypes["Upozorneni"],
	/** Reads a single `UpozorneniSkupiny` using its globally unique `ID`. */
	upozorneniSkupinyByNodeId?: GraphQLTypes["UpozorneniSkupiny"],
	/** Reads a single `User` using its globally unique `ID`. */
	userByNodeId?: GraphQLTypes["User"],
	/** Reads a single `UsersSkupiny` using its globally unique `ID`. */
	usersSkupinyByNodeId?: GraphQLTypes["UsersSkupiny"],
	/** Reads a single `Video` using its globally unique `ID`. */
	videoByNodeId?: GraphQLTypes["Video"],
	/** Reads a single `VideoList` using its globally unique `ID`. */
	videoListByNodeId?: GraphQLTypes["VideoList"],
	/** Reads a single `VideoSource` using its globally unique `ID`. */
	videoSourceByNodeId?: GraphQLTypes["VideoSource"]
};
	/** An object with a globally unique `ID`. */
["Node"]: {
	__typename:"Query" | "Akce" | "AkceItem" | "User" | "Permission" | "Skupiny" | "PlatbyGroupSkupina" | "PlatbyGroup" | "PlatbyCategoryGroup" | "PlatbyCategory" | "PlatbyItem" | "PlatbyRaw" | "UpozorneniSkupiny" | "Upozorneni" | "Aktuality" | "GalerieFoto" | "GalerieDir" | "Nabidka" | "NabidkaItem" | "Pary" | "RozpisItem" | "Rozpi" | "Session" | "Dokumenty" | "ParyNavrh" | "Page" | "PageRevision" | "Parameter" | "UsersSkupiny" | "Video" | "VideoList" | "VideoSource",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string
	['...on Query']: '__union' & GraphQLTypes["Query"];
	['...on Akce']: '__union' & GraphQLTypes["Akce"];
	['...on AkceItem']: '__union' & GraphQLTypes["AkceItem"];
	['...on User']: '__union' & GraphQLTypes["User"];
	['...on Permission']: '__union' & GraphQLTypes["Permission"];
	['...on Skupiny']: '__union' & GraphQLTypes["Skupiny"];
	['...on PlatbyGroupSkupina']: '__union' & GraphQLTypes["PlatbyGroupSkupina"];
	['...on PlatbyGroup']: '__union' & GraphQLTypes["PlatbyGroup"];
	['...on PlatbyCategoryGroup']: '__union' & GraphQLTypes["PlatbyCategoryGroup"];
	['...on PlatbyCategory']: '__union' & GraphQLTypes["PlatbyCategory"];
	['...on PlatbyItem']: '__union' & GraphQLTypes["PlatbyItem"];
	['...on PlatbyRaw']: '__union' & GraphQLTypes["PlatbyRaw"];
	['...on UpozorneniSkupiny']: '__union' & GraphQLTypes["UpozorneniSkupiny"];
	['...on Upozorneni']: '__union' & GraphQLTypes["Upozorneni"];
	['...on Aktuality']: '__union' & GraphQLTypes["Aktuality"];
	['...on GalerieFoto']: '__union' & GraphQLTypes["GalerieFoto"];
	['...on GalerieDir']: '__union' & GraphQLTypes["GalerieDir"];
	['...on Nabidka']: '__union' & GraphQLTypes["Nabidka"];
	['...on NabidkaItem']: '__union' & GraphQLTypes["NabidkaItem"];
	['...on Pary']: '__union' & GraphQLTypes["Pary"];
	['...on RozpisItem']: '__union' & GraphQLTypes["RozpisItem"];
	['...on Rozpi']: '__union' & GraphQLTypes["Rozpi"];
	['...on Session']: '__union' & GraphQLTypes["Session"];
	['...on Dokumenty']: '__union' & GraphQLTypes["Dokumenty"];
	['...on ParyNavrh']: '__union' & GraphQLTypes["ParyNavrh"];
	['...on Page']: '__union' & GraphQLTypes["Page"];
	['...on PageRevision']: '__union' & GraphQLTypes["PageRevision"];
	['...on Parameter']: '__union' & GraphQLTypes["Parameter"];
	['...on UsersSkupiny']: '__union' & GraphQLTypes["UsersSkupiny"];
	['...on Video']: '__union' & GraphQLTypes["Video"];
	['...on VideoList']: '__union' & GraphQLTypes["VideoList"];
	['...on VideoSource']: '__union' & GraphQLTypes["VideoSource"];
};
	/** A connection to a list of `Akce` values. */
["AkcesConnection"]: {
	__typename: "AkcesConnection",
	/** A list of `Akce` objects. */
	nodes: Array<GraphQLTypes["Akce"]>,
	/** A list of edges which contains the `Akce` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AkcesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Akce` you could get from the connection. */
	totalCount: number
};
	["Akce"]: {
	__typename: "Akce",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	aId: GraphQLTypes["BigInt"],
	aJmeno: string,
	aKde: string,
	aInfo: string,
	aOd: GraphQLTypes["Date"],
	aDo: GraphQLTypes["Date"],
	aKapacita: GraphQLTypes["BigInt"],
	aDokumenty: string,
	aTimestamp?: GraphQLTypes["Datetime"],
	aLock: boolean,
	aVisible: boolean,
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiIdRodic: GraphQLTypes["AkceItemsConnection"]
};
	/** A signed eight-byte integer. The upper big integer values are greater than the
max value for a JavaScript number. Therefore all big integers will be output as
strings and not numbers. */
["BigInt"]:any;
	/** The day, does not include a time. */
["Date"]:any;
	/** A point in time as described by the [ISO
8601](https://en.wikipedia.org/wiki/ISO_8601) standard. May or may not include a timezone. */
["Datetime"]:any;
	/** A connection to a list of `AkceItem` values. */
["AkceItemsConnection"]: {
	__typename: "AkceItemsConnection",
	/** A list of `AkceItem` objects. */
	nodes: Array<GraphQLTypes["AkceItem"]>,
	/** A list of edges which contains the `AkceItem` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AkceItemsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `AkceItem` you could get from the connection. */
	totalCount: number
};
	["AkceItem"]: {
	__typename: "AkceItem",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	aiId: GraphQLTypes["BigInt"],
	aiIdRodic: GraphQLTypes["BigInt"],
	aiUser: GraphQLTypes["BigInt"],
	aiRokNarozeni: number,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?: GraphQLTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?: GraphQLTypes["User"]
};
	["User"]: {
	__typename: "User",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	uId: GraphQLTypes["BigInt"],
	uLogin: string,
	uPass: string,
	uJmeno: string,
	uPrijmeni: string,
	uPohlavi: string,
	uEmail: string,
	uTelefon: string,
	uNarozeni: GraphQLTypes["Date"],
	uRodneCislo?: string,
	uPoznamky: string,
	uTimestamp: GraphQLTypes["Datetime"],
	uLevel: number,
	uGroup: GraphQLTypes["BigInt"],
	uSkupina: GraphQLTypes["BigInt"],
	uDancer: boolean,
	uBan: boolean,
	uLock: boolean,
	uConfirmed: boolean,
	uSystem: boolean,
	uStreet: string,
	uConscriptionNumber: string,
	uOrientationNumber: string,
	uDistrict: string,
	uCity: string,
	uPostalCode: string,
	uNationality: string,
	uMemberSince?: GraphQLTypes["Datetime"],
	uMemberUntil?: GraphQLTypes["Datetime"],
	uCreatedAt: GraphQLTypes["Datetime"],
	uTeacher: boolean,
	uGdprSignedAt?: GraphQLTypes["Datetime"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?: GraphQLTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?: GraphQLTypes["Skupiny"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtKdo: GraphQLTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkasByNTrener: GraphQLTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpisByRTrener: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessionsBySsUser: GraphQLTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiUser: GraphQLTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumentiesByDKdo: GraphQLTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfKdo: GraphQLTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdUser: GraphQLTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartner: GraphQLTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnNavrhl: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartner: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartnerka: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenisByUpKdo: GraphQLTypes["UpozornenisConnection"]
};
	["Permission"]: {
	__typename: "Permission",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	peId: GraphQLTypes["BigInt"],
	peName: string,
	peDescription: string,
	peAkce: number,
	peAktuality: number,
	peAnkety: number,
	peDokumenty: number,
	peGalerie: number,
	peInzerce: number,
	peKonzole: number,
	peNabidka: number,
	peNastenka: number,
	peNovinky: number,
	pePary: number,
	pePlatby: number,
	pePermissions: number,
	peRozpis: number,
	peSkupiny: number,
	peUsers: number,
	peMain: number,
	/** Reads and enables pagination through a set of `User`. */
	usersByUGroup: GraphQLTypes["UsersConnection"]
};
	/** A connection to a list of `User` values. */
["UsersConnection"]: {
	__typename: "UsersConnection",
	/** A list of `User` objects. */
	nodes: Array<GraphQLTypes["User"]>,
	/** A list of edges which contains the `User` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["UsersEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `User` you could get from the connection. */
	totalCount: number
};
	/** A `User` edge in the connection. */
["UsersEdge"]: {
	__typename: "UsersEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `User` at the end of the edge. */
	node: GraphQLTypes["User"]
};
	/** A location in a connection that can be used for resuming pagination. */
["Cursor"]:any;
	/** Information about pagination in a connection. */
["PageInfo"]: {
	__typename: "PageInfo",
	/** When paginating forwards, are there more items? */
	hasNextPage: boolean,
	/** When paginating backwards, are there more items? */
	hasPreviousPage: boolean,
	/** When paginating backwards, the cursor to continue. */
	startCursor?: GraphQLTypes["Cursor"],
	/** When paginating forwards, the cursor to continue. */
	endCursor?: GraphQLTypes["Cursor"]
};
	/** Methods to use when ordering `User`. */
["UsersOrderBy"]: UsersOrderBy;
	/** A condition to be used against `User` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["UserCondition"]: {
		/** Checks for equality with the object’s `uId` field. */
	uId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uLogin` field. */
	uLogin?: string,
	/** Checks for equality with the object’s `uPass` field. */
	uPass?: string,
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?: string,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?: string,
	/** Checks for equality with the object’s `uPohlavi` field. */
	uPohlavi?: string,
	/** Checks for equality with the object’s `uEmail` field. */
	uEmail?: string,
	/** Checks for equality with the object’s `uTelefon` field. */
	uTelefon?: string,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `uRodneCislo` field. */
	uRodneCislo?: string,
	/** Checks for equality with the object’s `uPoznamky` field. */
	uPoznamky?: string,
	/** Checks for equality with the object’s `uTimestamp` field. */
	uTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uLevel` field. */
	uLevel?: number,
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uDancer` field. */
	uDancer?: boolean,
	/** Checks for equality with the object’s `uBan` field. */
	uBan?: boolean,
	/** Checks for equality with the object’s `uLock` field. */
	uLock?: boolean,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?: boolean,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?: boolean,
	/** Checks for equality with the object’s `uStreet` field. */
	uStreet?: string,
	/** Checks for equality with the object’s `uConscriptionNumber` field. */
	uConscriptionNumber?: string,
	/** Checks for equality with the object’s `uOrientationNumber` field. */
	uOrientationNumber?: string,
	/** Checks for equality with the object’s `uDistrict` field. */
	uDistrict?: string,
	/** Checks for equality with the object’s `uCity` field. */
	uCity?: string,
	/** Checks for equality with the object’s `uPostalCode` field. */
	uPostalCode?: string,
	/** Checks for equality with the object’s `uNationality` field. */
	uNationality?: string,
	/** Checks for equality with the object’s `uMemberSince` field. */
	uMemberSince?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uMemberUntil` field. */
	uMemberUntil?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uCreatedAt` field. */
	uCreatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uTeacher` field. */
	uTeacher?: boolean,
	/** Checks for equality with the object’s `uGdprSignedAt` field. */
	uGdprSignedAt?: GraphQLTypes["Datetime"]
};
	["Skupiny"]: {
	__typename: "Skupiny",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	sId: GraphQLTypes["BigInt"],
	sName: string,
	sDescription: string,
	sColorRgb: string,
	sColorText: string,
	sLocation: string,
	sVisible: boolean,
	/** Reads and enables pagination through a set of `User`. */
	usersByUSkupina: GraphQLTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdSkupina: GraphQLTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdSkupina: GraphQLTypes["UpozorneniSkupiniesConnection"]
};
	/** A connection to a list of `PlatbyGroupSkupina` values. */
["PlatbyGroupSkupinasConnection"]: {
	__typename: "PlatbyGroupSkupinasConnection",
	/** A list of `PlatbyGroupSkupina` objects. */
	nodes: Array<GraphQLTypes["PlatbyGroupSkupina"]>,
	/** A list of edges which contains the `PlatbyGroupSkupina` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyGroupSkupinasEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyGroupSkupina` you could get from the connection. */
	totalCount: number
};
	["PlatbyGroupSkupina"]: {
	__typename: "PlatbyGroupSkupina",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pgsId: GraphQLTypes["BigInt"],
	pgsIdSkupina: GraphQLTypes["BigInt"],
	pgsIdGroup: GraphQLTypes["BigInt"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?: GraphQLTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?: GraphQLTypes["PlatbyGroup"]
};
	["PlatbyGroup"]: {
	__typename: "PlatbyGroup",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pgId: GraphQLTypes["BigInt"],
	pgType: GraphQLTypes["BigFloat"],
	pgName: string,
	pgDescription: string,
	pgBase: GraphQLTypes["BigInt"],
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroupsByPcgIdGroup: GraphQLTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdGroup: GraphQLTypes["PlatbyGroupSkupinasConnection"]
};
	/** A floating point number that requires more precision than IEEE 754 binary 64 */
["BigFloat"]:any;
	/** A connection to a list of `PlatbyCategoryGroup` values. */
["PlatbyCategoryGroupsConnection"]: {
	__typename: "PlatbyCategoryGroupsConnection",
	/** A list of `PlatbyCategoryGroup` objects. */
	nodes: Array<GraphQLTypes["PlatbyCategoryGroup"]>,
	/** A list of edges which contains the `PlatbyCategoryGroup` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyCategoryGroupsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyCategoryGroup` you could get from the connection. */
	totalCount: number
};
	["PlatbyCategoryGroup"]: {
	__typename: "PlatbyCategoryGroup",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pcgId: GraphQLTypes["BigInt"],
	pcgIdGroup: GraphQLTypes["BigInt"],
	pcgIdCategory: GraphQLTypes["BigInt"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?: GraphQLTypes["PlatbyCategory"]
};
	["PlatbyCategory"]: {
	__typename: "PlatbyCategory",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pcId: GraphQLTypes["BigInt"],
	pcName: string,
	pcSymbol: GraphQLTypes["BigInt"],
	pcAmount: GraphQLTypes["BigFloat"],
	pcDateDue: GraphQLTypes["Date"],
	pcValidFrom: GraphQLTypes["Date"],
	pcValidTo: GraphQLTypes["Date"],
	pcUseBase: boolean,
	pcUsePrefix: boolean,
	pcArchive: boolean,
	pcVisible: boolean,
	/** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
	platbyCategoryGroupsByPcgIdCategory: GraphQLTypes["PlatbyCategoryGroupsConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdCategory: GraphQLTypes["PlatbyItemsConnection"]
};
	/** Methods to use when ordering `PlatbyCategoryGroup`. */
["PlatbyCategoryGroupsOrderBy"]: PlatbyCategoryGroupsOrderBy;
	/** A condition to be used against `PlatbyCategoryGroup` object types. All fields
are tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryGroupCondition"]: {
		/** Checks for equality with the object’s `pcgId` field. */
	pcgId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pcgIdGroup` field. */
	pcgIdGroup?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pcgIdCategory` field. */
	pcgIdCategory?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `PlatbyItem` values. */
["PlatbyItemsConnection"]: {
	__typename: "PlatbyItemsConnection",
	/** A list of `PlatbyItem` objects. */
	nodes: Array<GraphQLTypes["PlatbyItem"]>,
	/** A list of edges which contains the `PlatbyItem` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyItemsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyItem` you could get from the connection. */
	totalCount: number
};
	["PlatbyItem"]: {
	__typename: "PlatbyItem",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	piId: GraphQLTypes["BigInt"],
	piIdUser?: GraphQLTypes["BigInt"],
	piIdCategory: GraphQLTypes["BigInt"],
	piIdRaw?: GraphQLTypes["BigInt"],
	piAmount: GraphQLTypes["BigFloat"],
	piDate: GraphQLTypes["Date"],
	piPrefix: number,
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?: GraphQLTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?: GraphQLTypes["PlatbyRaw"]
};
	["PlatbyRaw"]: {
	__typename: "PlatbyRaw",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	prId: GraphQLTypes["BigInt"],
	prRaw: string,
	prHash: string,
	prSorted: boolean,
	prDiscarded: boolean,
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdRaw: GraphQLTypes["PlatbyItemsConnection"]
};
	/** Methods to use when ordering `PlatbyItem`. */
["PlatbyItemsOrderBy"]: PlatbyItemsOrderBy;
	/** A condition to be used against `PlatbyItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyItemCondition"]: {
		/** Checks for equality with the object’s `piId` field. */
	piId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `piIdUser` field. */
	piIdUser?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `piIdCategory` field. */
	piIdCategory?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `piIdRaw` field. */
	piIdRaw?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `piAmount` field. */
	piAmount?: GraphQLTypes["BigFloat"],
	/** Checks for equality with the object’s `piDate` field. */
	piDate?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `piPrefix` field. */
	piPrefix?: number
};
	/** A `PlatbyItem` edge in the connection. */
["PlatbyItemsEdge"]: {
	__typename: "PlatbyItemsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyItem` at the end of the edge. */
	node: GraphQLTypes["PlatbyItem"]
};
	/** A `PlatbyCategoryGroup` edge in the connection. */
["PlatbyCategoryGroupsEdge"]: {
	__typename: "PlatbyCategoryGroupsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyCategoryGroup` at the end of the edge. */
	node: GraphQLTypes["PlatbyCategoryGroup"]
};
	/** Methods to use when ordering `PlatbyGroupSkupina`. */
["PlatbyGroupSkupinasOrderBy"]: PlatbyGroupSkupinasOrderBy;
	/** A condition to be used against `PlatbyGroupSkupina` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyGroupSkupinaCondition"]: {
		/** Checks for equality with the object’s `pgsId` field. */
	pgsId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pgsIdSkupina` field. */
	pgsIdSkupina?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pgsIdGroup` field. */
	pgsIdGroup?: GraphQLTypes["BigInt"]
};
	/** A `PlatbyGroupSkupina` edge in the connection. */
["PlatbyGroupSkupinasEdge"]: {
	__typename: "PlatbyGroupSkupinasEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyGroupSkupina` at the end of the edge. */
	node: GraphQLTypes["PlatbyGroupSkupina"]
};
	/** A connection to a list of `UpozorneniSkupiny` values. */
["UpozorneniSkupiniesConnection"]: {
	__typename: "UpozorneniSkupiniesConnection",
	/** A list of `UpozorneniSkupiny` objects. */
	nodes: Array<GraphQLTypes["UpozorneniSkupiny"]>,
	/** A list of edges which contains the `UpozorneniSkupiny` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["UpozorneniSkupiniesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `UpozorneniSkupiny` you could get from the connection. */
	totalCount: number
};
	["UpozorneniSkupiny"]: {
	__typename: "UpozorneniSkupiny",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	upsId: GraphQLTypes["BigInt"],
	upsIdRodic: GraphQLTypes["BigInt"],
	upsIdSkupina: GraphQLTypes["BigInt"],
	upsColor: string,
	upsPopis: string,
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?: GraphQLTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?: GraphQLTypes["Skupiny"]
};
	["Upozorneni"]: {
	__typename: "Upozorneni",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	upId: GraphQLTypes["BigInt"],
	upKdo: GraphQLTypes["BigInt"],
	upNadpis: string,
	upText: string,
	upBarvy: GraphQLTypes["BigInt"],
	upLock: boolean,
	upTimestamp?: GraphQLTypes["Datetime"],
	upTimestampAdd: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdRodic: GraphQLTypes["UpozorneniSkupiniesConnection"]
};
	/** Methods to use when ordering `UpozorneniSkupiny`. */
["UpozorneniSkupiniesOrderBy"]: UpozorneniSkupiniesOrderBy;
	/** A condition to be used against `UpozorneniSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UpozorneniSkupinyCondition"]: {
		/** Checks for equality with the object’s `upsId` field. */
	upsId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upsIdRodic` field. */
	upsIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upsIdSkupina` field. */
	upsIdSkupina?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upsColor` field. */
	upsColor?: string,
	/** Checks for equality with the object’s `upsPopis` field. */
	upsPopis?: string
};
	/** A `UpozorneniSkupiny` edge in the connection. */
["UpozorneniSkupiniesEdge"]: {
	__typename: "UpozorneniSkupiniesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `UpozorneniSkupiny` at the end of the edge. */
	node: GraphQLTypes["UpozorneniSkupiny"]
};
	/** A connection to a list of `Aktuality` values. */
["AktualitiesConnection"]: {
	__typename: "AktualitiesConnection",
	/** A list of `Aktuality` objects. */
	nodes: Array<GraphQLTypes["Aktuality"]>,
	/** A list of edges which contains the `Aktuality` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AktualitiesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Aktuality` you could get from the connection. */
	totalCount: number
};
	["Aktuality"]: {
	__typename: "Aktuality",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	atId: GraphQLTypes["BigInt"],
	atKdo: GraphQLTypes["BigInt"],
	atKat: string,
	atJmeno: string,
	atText: string,
	atPreview: string,
	atFoto?: GraphQLTypes["BigInt"],
	atFotoMain?: GraphQLTypes["BigInt"],
	atTimestamp?: GraphQLTypes["Datetime"],
	atTimestampAdd?: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?: GraphQLTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?: GraphQLTypes["GalerieFoto"]
};
	["GalerieFoto"]: {
	__typename: "GalerieFoto",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	gfId: GraphQLTypes["BigInt"],
	gfIdRodic: GraphQLTypes["BigInt"],
	gfName: string,
	gfPath: string,
	gfKdo: GraphQLTypes["BigInt"],
	gfTimestamp?: GraphQLTypes["Datetime"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?: GraphQLTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtFotoMain: GraphQLTypes["AktualitiesConnection"]
};
	["GalerieDir"]: {
	__typename: "GalerieDir",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	gdId: GraphQLTypes["BigInt"],
	gdIdRodic: GraphQLTypes["BigInt"],
	gdName: string,
	gdLevel: number,
	gdPath: string,
	gdHidden: boolean,
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfIdRodic: GraphQLTypes["GalerieFotosConnection"]
};
	/** A connection to a list of `GalerieFoto` values. */
["GalerieFotosConnection"]: {
	__typename: "GalerieFotosConnection",
	/** A list of `GalerieFoto` objects. */
	nodes: Array<GraphQLTypes["GalerieFoto"]>,
	/** A list of edges which contains the `GalerieFoto` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["GalerieFotosEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `GalerieFoto` you could get from the connection. */
	totalCount: number
};
	/** A `GalerieFoto` edge in the connection. */
["GalerieFotosEdge"]: {
	__typename: "GalerieFotosEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `GalerieFoto` at the end of the edge. */
	node: GraphQLTypes["GalerieFoto"]
};
	/** Methods to use when ordering `GalerieFoto`. */
["GalerieFotosOrderBy"]: GalerieFotosOrderBy;
	/** A condition to be used against `GalerieFoto` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieFotoCondition"]: {
		/** Checks for equality with the object’s `gfId` field. */
	gfId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `gfIdRodic` field. */
	gfIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `gfName` field. */
	gfName?: string,
	/** Checks for equality with the object’s `gfPath` field. */
	gfPath?: string,
	/** Checks for equality with the object’s `gfKdo` field. */
	gfKdo?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `gfTimestamp` field. */
	gfTimestamp?: GraphQLTypes["Datetime"]
};
	/** Methods to use when ordering `Aktuality`. */
["AktualitiesOrderBy"]: AktualitiesOrderBy;
	/** A condition to be used against `Aktuality` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AktualityCondition"]: {
		/** Checks for equality with the object’s `atId` field. */
	atId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `atKdo` field. */
	atKdo?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `atKat` field. */
	atKat?: string,
	/** Checks for equality with the object’s `atJmeno` field. */
	atJmeno?: string,
	/** Checks for equality with the object’s `atText` field. */
	atText?: string,
	/** Checks for equality with the object’s `atPreview` field. */
	atPreview?: string,
	/** Checks for equality with the object’s `atFoto` field. */
	atFoto?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `atFotoMain` field. */
	atFotoMain?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `atTimestamp` field. */
	atTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `atTimestampAdd` field. */
	atTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** A `Aktuality` edge in the connection. */
["AktualitiesEdge"]: {
	__typename: "AktualitiesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Aktuality` at the end of the edge. */
	node: GraphQLTypes["Aktuality"]
};
	/** A connection to a list of `Nabidka` values. */
["NabidkasConnection"]: {
	__typename: "NabidkasConnection",
	/** A list of `Nabidka` objects. */
	nodes: Array<GraphQLTypes["Nabidka"]>,
	/** A list of edges which contains the `Nabidka` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["NabidkasEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Nabidka` you could get from the connection. */
	totalCount: number
};
	["Nabidka"]: {
	__typename: "Nabidka",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	nId: GraphQLTypes["BigInt"],
	nTrener: GraphQLTypes["BigInt"],
	nPocetHod: number,
	nMaxPocetHod: GraphQLTypes["BigInt"],
	nOd: GraphQLTypes["Date"],
	nDo: GraphQLTypes["Date"],
	nVisible: boolean,
	nLock: boolean,
	nTimestamp?: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiIdRodic: GraphQLTypes["NabidkaItemsConnection"]
};
	/** A connection to a list of `NabidkaItem` values. */
["NabidkaItemsConnection"]: {
	__typename: "NabidkaItemsConnection",
	/** A list of `NabidkaItem` objects. */
	nodes: Array<GraphQLTypes["NabidkaItem"]>,
	/** A list of edges which contains the `NabidkaItem` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["NabidkaItemsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `NabidkaItem` you could get from the connection. */
	totalCount: number
};
	["NabidkaItem"]: {
	__typename: "NabidkaItem",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	niId: GraphQLTypes["BigInt"],
	niIdRodic: GraphQLTypes["BigInt"],
	niPartner: GraphQLTypes["BigInt"],
	niPocetHod: number,
	niLock: boolean,
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?: GraphQLTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?: GraphQLTypes["Pary"]
};
	["Pary"]: {
	__typename: "Pary",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pId: GraphQLTypes["BigInt"],
	pIdPartner: GraphQLTypes["BigInt"],
	pIdPartnerka?: GraphQLTypes["BigInt"],
	pSttTrida: GraphQLTypes["ParyPSttTrida"],
	pSttBody: number,
	pSttFinale: boolean,
	pLatTrida: GraphQLTypes["ParyPLatTrida"],
	pLatBody: number,
	pLatFinale: boolean,
	pHodnoceni: number,
	pArchiv: boolean,
	pTimestampAdd: GraphQLTypes["Datetime"],
	pTimestampArchive?: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiPartner: GraphQLTypes["NabidkaItemsConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItemsByRiPartner: GraphQLTypes["RozpisItemsConnection"]
};
	["ParyPSttTrida"]: ParyPSttTrida;
	["ParyPLatTrida"]: ParyPLatTrida;
	/** Methods to use when ordering `NabidkaItem`. */
["NabidkaItemsOrderBy"]: NabidkaItemsOrderBy;
	/** A condition to be used against `NabidkaItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["NabidkaItemCondition"]: {
		/** Checks for equality with the object’s `niId` field. */
	niId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `niIdRodic` field. */
	niIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `niPartner` field. */
	niPartner?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `niPocetHod` field. */
	niPocetHod?: number,
	/** Checks for equality with the object’s `niLock` field. */
	niLock?: boolean
};
	/** A connection to a list of `RozpisItem` values. */
["RozpisItemsConnection"]: {
	__typename: "RozpisItemsConnection",
	/** A list of `RozpisItem` objects. */
	nodes: Array<GraphQLTypes["RozpisItem"]>,
	/** A list of edges which contains the `RozpisItem` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["RozpisItemsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `RozpisItem` you could get from the connection. */
	totalCount: number
};
	["RozpisItem"]: {
	__typename: "RozpisItem",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	riId: GraphQLTypes["BigInt"],
	riIdRodic: GraphQLTypes["BigInt"],
	riPartner?: GraphQLTypes["BigInt"],
	riOd: GraphQLTypes["Time"],
	riDo: GraphQLTypes["Time"],
	riLock: boolean,
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?: GraphQLTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?: GraphQLTypes["Pary"]
};
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:any;
	["Rozpi"]: {
	__typename: "Rozpi",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	rId: GraphQLTypes["BigInt"],
	rTrener: GraphQLTypes["BigInt"],
	rKde: string,
	rDatum: GraphQLTypes["Date"],
	rVisible: boolean,
	rLock: boolean,
	rTimestamp?: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItemsByRiIdRodic: GraphQLTypes["RozpisItemsConnection"]
};
	/** Methods to use when ordering `RozpisItem`. */
["RozpisItemsOrderBy"]: RozpisItemsOrderBy;
	/** A condition to be used against `RozpisItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["RozpisItemCondition"]: {
		/** Checks for equality with the object’s `riId` field. */
	riId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `riIdRodic` field. */
	riIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `riPartner` field. */
	riPartner?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `riOd` field. */
	riOd?: GraphQLTypes["Time"],
	/** Checks for equality with the object’s `riDo` field. */
	riDo?: GraphQLTypes["Time"],
	/** Checks for equality with the object’s `riLock` field. */
	riLock?: boolean
};
	/** A `RozpisItem` edge in the connection. */
["RozpisItemsEdge"]: {
	__typename: "RozpisItemsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `RozpisItem` at the end of the edge. */
	node: GraphQLTypes["RozpisItem"]
};
	/** A `NabidkaItem` edge in the connection. */
["NabidkaItemsEdge"]: {
	__typename: "NabidkaItemsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `NabidkaItem` at the end of the edge. */
	node: GraphQLTypes["NabidkaItem"]
};
	/** A `Nabidka` edge in the connection. */
["NabidkasEdge"]: {
	__typename: "NabidkasEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Nabidka` at the end of the edge. */
	node: GraphQLTypes["Nabidka"]
};
	/** Methods to use when ordering `Nabidka`. */
["NabidkasOrderBy"]: NabidkasOrderBy;
	/** A condition to be used against `Nabidka` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["NabidkaCondition"]: {
		/** Checks for equality with the object’s `nId` field. */
	nId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `nTrener` field. */
	nTrener?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `nPocetHod` field. */
	nPocetHod?: number,
	/** Checks for equality with the object’s `nMaxPocetHod` field. */
	nMaxPocetHod?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `nOd` field. */
	nOd?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `nDo` field. */
	nDo?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `nVisible` field. */
	nVisible?: boolean,
	/** Checks for equality with the object’s `nLock` field. */
	nLock?: boolean,
	/** Checks for equality with the object’s `nTimestamp` field. */
	nTimestamp?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `Rozpi` values. */
["RozpisConnection"]: {
	__typename: "RozpisConnection",
	/** A list of `Rozpi` objects. */
	nodes: Array<GraphQLTypes["Rozpi"]>,
	/** A list of edges which contains the `Rozpi` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["RozpisEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Rozpi` you could get from the connection. */
	totalCount: number
};
	/** A `Rozpi` edge in the connection. */
["RozpisEdge"]: {
	__typename: "RozpisEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Rozpi` at the end of the edge. */
	node: GraphQLTypes["Rozpi"]
};
	/** Methods to use when ordering `Rozpi`. */
["RozpisOrderBy"]: RozpisOrderBy;
	/** A condition to be used against `Rozpi` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RozpiCondition"]: {
		/** Checks for equality with the object’s `rId` field. */
	rId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `rTrener` field. */
	rTrener?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `rKde` field. */
	rKde?: string,
	/** Checks for equality with the object’s `rDatum` field. */
	rDatum?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `rVisible` field. */
	rVisible?: boolean,
	/** Checks for equality with the object’s `rLock` field. */
	rLock?: boolean,
	/** Checks for equality with the object’s `rTimestamp` field. */
	rTimestamp?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `Session` values. */
["SessionsConnection"]: {
	__typename: "SessionsConnection",
	/** A list of `Session` objects. */
	nodes: Array<GraphQLTypes["Session"]>,
	/** A list of edges which contains the `Session` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["SessionsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Session` you could get from the connection. */
	totalCount: number
};
	["Session"]: {
	__typename: "Session",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	ssId: string,
	ssData: string,
	ssUpdatedAt: GraphQLTypes["Datetime"],
	ssLifetime: GraphQLTypes["BigInt"],
	ssUser?: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?: GraphQLTypes["User"]
};
	/** A `Session` edge in the connection. */
["SessionsEdge"]: {
	__typename: "SessionsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Session` at the end of the edge. */
	node: GraphQLTypes["Session"]
};
	/** Methods to use when ordering `Session`. */
["SessionsOrderBy"]: SessionsOrderBy;
	/** A condition to be used against `Session` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SessionCondition"]: {
		/** Checks for equality with the object’s `ssId` field. */
	ssId?: string,
	/** Checks for equality with the object’s `ssData` field. */
	ssData?: string,
	/** Checks for equality with the object’s `ssUpdatedAt` field. */
	ssUpdatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `ssLifetime` field. */
	ssLifetime?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `ssUser` field. */
	ssUser?: GraphQLTypes["BigInt"]
};
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]: AkceItemsOrderBy;
	/** A condition to be used against `AkceItem` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AkceItemCondition"]: {
		/** Checks for equality with the object’s `aiId` field. */
	aiId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `aiIdRodic` field. */
	aiIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `aiUser` field. */
	aiUser?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `aiRokNarozeni` field. */
	aiRokNarozeni?: number
};
	/** A connection to a list of `Dokumenty` values. */
["DokumentiesConnection"]: {
	__typename: "DokumentiesConnection",
	/** A list of `Dokumenty` objects. */
	nodes: Array<GraphQLTypes["Dokumenty"]>,
	/** A list of edges which contains the `Dokumenty` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["DokumentiesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Dokumenty` you could get from the connection. */
	totalCount: number
};
	["Dokumenty"]: {
	__typename: "Dokumenty",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	dId: GraphQLTypes["BigInt"],
	dPath: string,
	dName: string,
	dFilename: string,
	dKategorie: number,
	dKdo: GraphQLTypes["BigInt"],
	dTimestamp?: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?: GraphQLTypes["User"]
};
	/** A `Dokumenty` edge in the connection. */
["DokumentiesEdge"]: {
	__typename: "DokumentiesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Dokumenty` at the end of the edge. */
	node: GraphQLTypes["Dokumenty"]
};
	/** Methods to use when ordering `Dokumenty`. */
["DokumentiesOrderBy"]: DokumentiesOrderBy;
	/** A condition to be used against `Dokumenty` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["DokumentyCondition"]: {
		/** Checks for equality with the object’s `dId` field. */
	dId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `dPath` field. */
	dPath?: string,
	/** Checks for equality with the object’s `dName` field. */
	dName?: string,
	/** Checks for equality with the object’s `dFilename` field. */
	dFilename?: string,
	/** Checks for equality with the object’s `dKategorie` field. */
	dKategorie?: number,
	/** Checks for equality with the object’s `dKdo` field. */
	dKdo?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `dTimestamp` field. */
	dTimestamp?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `Pary` values. */
["PariesConnection"]: {
	__typename: "PariesConnection",
	/** A list of `Pary` objects. */
	nodes: Array<GraphQLTypes["Pary"]>,
	/** A list of edges which contains the `Pary` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PariesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Pary` you could get from the connection. */
	totalCount: number
};
	/** A `Pary` edge in the connection. */
["PariesEdge"]: {
	__typename: "PariesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Pary` at the end of the edge. */
	node: GraphQLTypes["Pary"]
};
	/** Methods to use when ordering `Pary`. */
["PariesOrderBy"]: PariesOrderBy;
	/** A condition to be used against `Pary` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["ParyCondition"]: {
		/** Checks for equality with the object’s `pId` field. */
	pId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pIdPartner` field. */
	pIdPartner?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pIdPartnerka` field. */
	pIdPartnerka?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pSttTrida` field. */
	pSttTrida?: GraphQLTypes["ParyPSttTrida"],
	/** Checks for equality with the object’s `pSttBody` field. */
	pSttBody?: number,
	/** Checks for equality with the object’s `pSttFinale` field. */
	pSttFinale?: boolean,
	/** Checks for equality with the object’s `pLatTrida` field. */
	pLatTrida?: GraphQLTypes["ParyPLatTrida"],
	/** Checks for equality with the object’s `pLatBody` field. */
	pLatBody?: number,
	/** Checks for equality with the object’s `pLatFinale` field. */
	pLatFinale?: boolean,
	/** Checks for equality with the object’s `pHodnoceni` field. */
	pHodnoceni?: number,
	/** Checks for equality with the object’s `pArchiv` field. */
	pArchiv?: boolean,
	/** Checks for equality with the object’s `pTimestampAdd` field. */
	pTimestampAdd?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `pTimestampArchive` field. */
	pTimestampArchive?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `ParyNavrh` values. */
["ParyNavrhsConnection"]: {
	__typename: "ParyNavrhsConnection",
	/** A list of `ParyNavrh` objects. */
	nodes: Array<GraphQLTypes["ParyNavrh"]>,
	/** A list of edges which contains the `ParyNavrh` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["ParyNavrhsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `ParyNavrh` you could get from the connection. */
	totalCount: number
};
	["ParyNavrh"]: {
	__typename: "ParyNavrh",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	pnId: GraphQLTypes["BigInt"],
	pnNavrhl: GraphQLTypes["BigInt"],
	pnPartner: GraphQLTypes["BigInt"],
	pnPartnerka: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?: GraphQLTypes["User"]
};
	/** A `ParyNavrh` edge in the connection. */
["ParyNavrhsEdge"]: {
	__typename: "ParyNavrhsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `ParyNavrh` at the end of the edge. */
	node: GraphQLTypes["ParyNavrh"]
};
	/** Methods to use when ordering `ParyNavrh`. */
["ParyNavrhsOrderBy"]: ParyNavrhsOrderBy;
	/** A condition to be used against `ParyNavrh` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParyNavrhCondition"]: {
		/** Checks for equality with the object’s `pnId` field. */
	pnId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pnNavrhl` field. */
	pnNavrhl?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pnPartner` field. */
	pnPartner?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pnPartnerka` field. */
	pnPartnerka?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `Upozorneni` values. */
["UpozornenisConnection"]: {
	__typename: "UpozornenisConnection",
	/** A list of `Upozorneni` objects. */
	nodes: Array<GraphQLTypes["Upozorneni"]>,
	/** A list of edges which contains the `Upozorneni` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["UpozornenisEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Upozorneni` you could get from the connection. */
	totalCount: number
};
	/** A `Upozorneni` edge in the connection. */
["UpozornenisEdge"]: {
	__typename: "UpozornenisEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Upozorneni` at the end of the edge. */
	node: GraphQLTypes["Upozorneni"]
};
	/** Methods to use when ordering `Upozorneni`. */
["UpozornenisOrderBy"]: UpozornenisOrderBy;
	/** A condition to be used against `Upozorneni` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["UpozorneniCondition"]: {
		/** Checks for equality with the object’s `upId` field. */
	upId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upKdo` field. */
	upKdo?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upNadpis` field. */
	upNadpis?: string,
	/** Checks for equality with the object’s `upText` field. */
	upText?: string,
	/** Checks for equality with the object’s `upBarvy` field. */
	upBarvy?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `upLock` field. */
	upLock?: boolean,
	/** Checks for equality with the object’s `upTimestamp` field. */
	upTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `upTimestampAdd` field. */
	upTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** A `AkceItem` edge in the connection. */
["AkceItemsEdge"]: {
	__typename: "AkceItemsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `AkceItem` at the end of the edge. */
	node: GraphQLTypes["AkceItem"]
};
	/** A `Akce` edge in the connection. */
["AkcesEdge"]: {
	__typename: "AkcesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Akce` at the end of the edge. */
	node: GraphQLTypes["Akce"]
};
	/** Methods to use when ordering `Akce`. */
["AkcesOrderBy"]: AkcesOrderBy;
	/** A condition to be used against `Akce` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["AkceCondition"]: {
		/** Checks for equality with the object’s `aId` field. */
	aId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `aJmeno` field. */
	aJmeno?: string,
	/** Checks for equality with the object’s `aKde` field. */
	aKde?: string,
	/** Checks for equality with the object’s `aInfo` field. */
	aInfo?: string,
	/** Checks for equality with the object’s `aOd` field. */
	aOd?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `aDo` field. */
	aDo?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `aKapacita` field. */
	aKapacita?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `aDokumenty` field. */
	aDokumenty?: string,
	/** Checks for equality with the object’s `aTimestamp` field. */
	aTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `aLock` field. */
	aLock?: boolean,
	/** Checks for equality with the object’s `aVisible` field. */
	aVisible?: boolean
};
	/** A connection to a list of `GalerieDir` values. */
["GalerieDirsConnection"]: {
	__typename: "GalerieDirsConnection",
	/** A list of `GalerieDir` objects. */
	nodes: Array<GraphQLTypes["GalerieDir"]>,
	/** A list of edges which contains the `GalerieDir` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["GalerieDirsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `GalerieDir` you could get from the connection. */
	totalCount: number
};
	/** A `GalerieDir` edge in the connection. */
["GalerieDirsEdge"]: {
	__typename: "GalerieDirsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `GalerieDir` at the end of the edge. */
	node: GraphQLTypes["GalerieDir"]
};
	/** Methods to use when ordering `GalerieDir`. */
["GalerieDirsOrderBy"]: GalerieDirsOrderBy;
	/** A condition to be used against `GalerieDir` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["GalerieDirCondition"]: {
		/** Checks for equality with the object’s `gdId` field. */
	gdId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `gdIdRodic` field. */
	gdIdRodic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `gdName` field. */
	gdName?: string,
	/** Checks for equality with the object’s `gdLevel` field. */
	gdLevel?: number,
	/** Checks for equality with the object’s `gdPath` field. */
	gdPath?: string,
	/** Checks for equality with the object’s `gdHidden` field. */
	gdHidden?: boolean
};
	/** A connection to a list of `Member` values. */
["MembersConnection"]: {
	__typename: "MembersConnection",
	/** A list of `Member` objects. */
	nodes: Array<GraphQLTypes["Member"]>,
	/** A list of edges which contains the `Member` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["MembersEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Member` you could get from the connection. */
	totalCount: number
};
	["Member"]: {
	__typename: "Member",
	uId?: GraphQLTypes["BigInt"],
	uLogin?: string,
	uPass?: string,
	uJmeno?: string,
	uPrijmeni?: string,
	uPohlavi?: string,
	uEmail?: string,
	uTelefon?: string,
	uNarozeni?: GraphQLTypes["Date"],
	uRodneCislo?: string,
	uPoznamky?: string,
	uTimestamp?: GraphQLTypes["Datetime"],
	uLevel?: number,
	uGroup?: GraphQLTypes["BigInt"],
	uSkupina?: GraphQLTypes["BigInt"],
	uDancer?: boolean,
	uBan?: boolean,
	uLock?: boolean,
	uConfirmed?: boolean,
	uSystem?: boolean,
	uStreet?: string,
	uConscriptionNumber?: string,
	uOrientationNumber?: string,
	uDistrict?: string,
	uCity?: string,
	uPostalCode?: string,
	uNationality?: string,
	uMemberSince?: GraphQLTypes["Datetime"],
	uMemberUntil?: GraphQLTypes["Datetime"],
	uCreatedAt?: GraphQLTypes["Datetime"],
	uTeacher?: boolean,
	uGdprSignedAt?: GraphQLTypes["Datetime"],
	sId?: GraphQLTypes["BigInt"],
	sName?: string,
	paymentValid?: boolean
};
	/** A `Member` edge in the connection. */
["MembersEdge"]: {
	__typename: "MembersEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Member` at the end of the edge. */
	node: GraphQLTypes["Member"]
};
	/** Methods to use when ordering `Member`. */
["MembersOrderBy"]: MembersOrderBy;
	/** A condition to be used against `Member` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["MemberCondition"]: {
		/** Checks for equality with the object’s `uId` field. */
	uId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uLogin` field. */
	uLogin?: string,
	/** Checks for equality with the object’s `uPass` field. */
	uPass?: string,
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?: string,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?: string,
	/** Checks for equality with the object’s `uPohlavi` field. */
	uPohlavi?: string,
	/** Checks for equality with the object’s `uEmail` field. */
	uEmail?: string,
	/** Checks for equality with the object’s `uTelefon` field. */
	uTelefon?: string,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `uRodneCislo` field. */
	uRodneCislo?: string,
	/** Checks for equality with the object’s `uPoznamky` field. */
	uPoznamky?: string,
	/** Checks for equality with the object’s `uTimestamp` field. */
	uTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uLevel` field. */
	uLevel?: number,
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uDancer` field. */
	uDancer?: boolean,
	/** Checks for equality with the object’s `uBan` field. */
	uBan?: boolean,
	/** Checks for equality with the object’s `uLock` field. */
	uLock?: boolean,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?: boolean,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?: boolean,
	/** Checks for equality with the object’s `uStreet` field. */
	uStreet?: string,
	/** Checks for equality with the object’s `uConscriptionNumber` field. */
	uConscriptionNumber?: string,
	/** Checks for equality with the object’s `uOrientationNumber` field. */
	uOrientationNumber?: string,
	/** Checks for equality with the object’s `uDistrict` field. */
	uDistrict?: string,
	/** Checks for equality with the object’s `uCity` field. */
	uCity?: string,
	/** Checks for equality with the object’s `uPostalCode` field. */
	uPostalCode?: string,
	/** Checks for equality with the object’s `uNationality` field. */
	uNationality?: string,
	/** Checks for equality with the object’s `uMemberSince` field. */
	uMemberSince?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uMemberUntil` field. */
	uMemberUntil?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uCreatedAt` field. */
	uCreatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `uTeacher` field. */
	uTeacher?: boolean,
	/** Checks for equality with the object’s `uGdprSignedAt` field. */
	uGdprSignedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `sId` field. */
	sId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `sName` field. */
	sName?: string,
	/** Checks for equality with the object’s `paymentValid` field. */
	paymentValid?: boolean
};
	/** A connection to a list of `Page` values. */
["PagesConnection"]: {
	__typename: "PagesConnection",
	/** A list of `Page` objects. */
	nodes: Array<GraphQLTypes["Page"]>,
	/** A list of edges which contains the `Page` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PagesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Page` you could get from the connection. */
	totalCount: number
};
	["Page"]: {
	__typename: "Page",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	id: number,
	url: string,
	content: GraphQLTypes["JSON"],
	createdAt: GraphQLTypes["Datetime"],
	updatedAt: GraphQLTypes["Datetime"],
	title: string
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	/** A `Page` edge in the connection. */
["PagesEdge"]: {
	__typename: "PagesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Page` at the end of the edge. */
	node: GraphQLTypes["Page"]
};
	/** Methods to use when ordering `Page`. */
["PagesOrderBy"]: PagesOrderBy;
	/** A condition to be used against `Page` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PageCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: number,
	/** Checks for equality with the object’s `url` field. */
	url?: string,
	/** Checks for equality with the object’s `content` field. */
	content?: GraphQLTypes["JSON"],
	/** Checks for equality with the object’s `createdAt` field. */
	createdAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `title` field. */
	title?: string
};
	/** A connection to a list of `PageRevision` values. */
["PageRevisionsConnection"]: {
	__typename: "PageRevisionsConnection",
	/** A list of `PageRevision` objects. */
	nodes: Array<GraphQLTypes["PageRevision"]>,
	/** A list of edges which contains the `PageRevision` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PageRevisionsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PageRevision` you could get from the connection. */
	totalCount: number
};
	["PageRevision"]: {
	__typename: "PageRevision",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	revNumber: number,
	revOperation: string,
	revTimestamp?: GraphQLTypes["Datetime"],
	id: number,
	url: string,
	content: GraphQLTypes["JSON"],
	createdAt: GraphQLTypes["Datetime"],
	updatedAt: GraphQLTypes["Datetime"],
	title: string
};
	/** A `PageRevision` edge in the connection. */
["PageRevisionsEdge"]: {
	__typename: "PageRevisionsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PageRevision` at the end of the edge. */
	node: GraphQLTypes["PageRevision"]
};
	/** Methods to use when ordering `PageRevision`. */
["PageRevisionsOrderBy"]: PageRevisionsOrderBy;
	/** A condition to be used against `PageRevision` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PageRevisionCondition"]: {
		/** Checks for equality with the object’s `revNumber` field. */
	revNumber?: number,
	/** Checks for equality with the object’s `revOperation` field. */
	revOperation?: string,
	/** Checks for equality with the object’s `revTimestamp` field. */
	revTimestamp?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `id` field. */
	id?: number,
	/** Checks for equality with the object’s `url` field. */
	url?: string,
	/** Checks for equality with the object’s `content` field. */
	content?: GraphQLTypes["JSON"],
	/** Checks for equality with the object’s `createdAt` field. */
	createdAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `title` field. */
	title?: string
};
	/** A connection to a list of `Parameter` values. */
["ParametersConnection"]: {
	__typename: "ParametersConnection",
	/** A list of `Parameter` objects. */
	nodes: Array<GraphQLTypes["Parameter"]>,
	/** A list of edges which contains the `Parameter` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["ParametersEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Parameter` you could get from the connection. */
	totalCount: number
};
	["Parameter"]: {
	__typename: "Parameter",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	paName: string,
	paValue: string
};
	/** A `Parameter` edge in the connection. */
["ParametersEdge"]: {
	__typename: "ParametersEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Parameter` at the end of the edge. */
	node: GraphQLTypes["Parameter"]
};
	/** Methods to use when ordering `Parameter`. */
["ParametersOrderBy"]: ParametersOrderBy;
	/** A condition to be used against `Parameter` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["ParameterCondition"]: {
		/** Checks for equality with the object’s `paName` field. */
	paName?: string,
	/** Checks for equality with the object’s `paValue` field. */
	paValue?: string
};
	/** A connection to a list of `Permission` values. */
["PermissionsConnection"]: {
	__typename: "PermissionsConnection",
	/** A list of `Permission` objects. */
	nodes: Array<GraphQLTypes["Permission"]>,
	/** A list of edges which contains the `Permission` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PermissionsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Permission` you could get from the connection. */
	totalCount: number
};
	/** A `Permission` edge in the connection. */
["PermissionsEdge"]: {
	__typename: "PermissionsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Permission` at the end of the edge. */
	node: GraphQLTypes["Permission"]
};
	/** Methods to use when ordering `Permission`. */
["PermissionsOrderBy"]: PermissionsOrderBy;
	/** A condition to be used against `Permission` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PermissionCondition"]: {
		/** Checks for equality with the object’s `peId` field. */
	peId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `peName` field. */
	peName?: string,
	/** Checks for equality with the object’s `peDescription` field. */
	peDescription?: string,
	/** Checks for equality with the object’s `peAkce` field. */
	peAkce?: number,
	/** Checks for equality with the object’s `peAktuality` field. */
	peAktuality?: number,
	/** Checks for equality with the object’s `peAnkety` field. */
	peAnkety?: number,
	/** Checks for equality with the object’s `peDokumenty` field. */
	peDokumenty?: number,
	/** Checks for equality with the object’s `peGalerie` field. */
	peGalerie?: number,
	/** Checks for equality with the object’s `peInzerce` field. */
	peInzerce?: number,
	/** Checks for equality with the object’s `peKonzole` field. */
	peKonzole?: number,
	/** Checks for equality with the object’s `peNabidka` field. */
	peNabidka?: number,
	/** Checks for equality with the object’s `peNastenka` field. */
	peNastenka?: number,
	/** Checks for equality with the object’s `peNovinky` field. */
	peNovinky?: number,
	/** Checks for equality with the object’s `pePary` field. */
	pePary?: number,
	/** Checks for equality with the object’s `pePlatby` field. */
	pePlatby?: number,
	/** Checks for equality with the object’s `pePermissions` field. */
	pePermissions?: number,
	/** Checks for equality with the object’s `peRozpis` field. */
	peRozpis?: number,
	/** Checks for equality with the object’s `peSkupiny` field. */
	peSkupiny?: number,
	/** Checks for equality with the object’s `peUsers` field. */
	peUsers?: number,
	/** Checks for equality with the object’s `peMain` field. */
	peMain?: number
};
	/** A connection to a list of `PlatbyCategory` values. */
["PlatbyCategoriesConnection"]: {
	__typename: "PlatbyCategoriesConnection",
	/** A list of `PlatbyCategory` objects. */
	nodes: Array<GraphQLTypes["PlatbyCategory"]>,
	/** A list of edges which contains the `PlatbyCategory` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyCategoriesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyCategory` you could get from the connection. */
	totalCount: number
};
	/** A `PlatbyCategory` edge in the connection. */
["PlatbyCategoriesEdge"]: {
	__typename: "PlatbyCategoriesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyCategory` at the end of the edge. */
	node: GraphQLTypes["PlatbyCategory"]
};
	/** Methods to use when ordering `PlatbyCategory`. */
["PlatbyCategoriesOrderBy"]: PlatbyCategoriesOrderBy;
	/** A condition to be used against `PlatbyCategory` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["PlatbyCategoryCondition"]: {
		/** Checks for equality with the object’s `pcId` field. */
	pcId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pcName` field. */
	pcName?: string,
	/** Checks for equality with the object’s `pcSymbol` field. */
	pcSymbol?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pcAmount` field. */
	pcAmount?: GraphQLTypes["BigFloat"],
	/** Checks for equality with the object’s `pcDateDue` field. */
	pcDateDue?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `pcValidFrom` field. */
	pcValidFrom?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `pcValidTo` field. */
	pcValidTo?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `pcUseBase` field. */
	pcUseBase?: boolean,
	/** Checks for equality with the object’s `pcUsePrefix` field. */
	pcUsePrefix?: boolean,
	/** Checks for equality with the object’s `pcArchive` field. */
	pcArchive?: boolean,
	/** Checks for equality with the object’s `pcVisible` field. */
	pcVisible?: boolean
};
	/** A connection to a list of `PlatbyGroup` values. */
["PlatbyGroupsConnection"]: {
	__typename: "PlatbyGroupsConnection",
	/** A list of `PlatbyGroup` objects. */
	nodes: Array<GraphQLTypes["PlatbyGroup"]>,
	/** A list of edges which contains the `PlatbyGroup` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyGroupsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyGroup` you could get from the connection. */
	totalCount: number
};
	/** A `PlatbyGroup` edge in the connection. */
["PlatbyGroupsEdge"]: {
	__typename: "PlatbyGroupsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyGroup` at the end of the edge. */
	node: GraphQLTypes["PlatbyGroup"]
};
	/** Methods to use when ordering `PlatbyGroup`. */
["PlatbyGroupsOrderBy"]: PlatbyGroupsOrderBy;
	/** A condition to be used against `PlatbyGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyGroupCondition"]: {
		/** Checks for equality with the object’s `pgId` field. */
	pgId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `pgType` field. */
	pgType?: GraphQLTypes["BigFloat"],
	/** Checks for equality with the object’s `pgName` field. */
	pgName?: string,
	/** Checks for equality with the object’s `pgDescription` field. */
	pgDescription?: string,
	/** Checks for equality with the object’s `pgBase` field. */
	pgBase?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `PlatbyRaw` values. */
["PlatbyRawsConnection"]: {
	__typename: "PlatbyRawsConnection",
	/** A list of `PlatbyRaw` objects. */
	nodes: Array<GraphQLTypes["PlatbyRaw"]>,
	/** A list of edges which contains the `PlatbyRaw` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PlatbyRawsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `PlatbyRaw` you could get from the connection. */
	totalCount: number
};
	/** A `PlatbyRaw` edge in the connection. */
["PlatbyRawsEdge"]: {
	__typename: "PlatbyRawsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `PlatbyRaw` at the end of the edge. */
	node: GraphQLTypes["PlatbyRaw"]
};
	/** Methods to use when ordering `PlatbyRaw`. */
["PlatbyRawsOrderBy"]: PlatbyRawsOrderBy;
	/** A condition to be used against `PlatbyRaw` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["PlatbyRawCondition"]: {
		/** Checks for equality with the object’s `prId` field. */
	prId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `prRaw` field. */
	prRaw?: string,
	/** Checks for equality with the object’s `prHash` field. */
	prHash?: string,
	/** Checks for equality with the object’s `prSorted` field. */
	prSorted?: boolean,
	/** Checks for equality with the object’s `prDiscarded` field. */
	prDiscarded?: boolean
};
	/** A connection to a list of `Skupiny` values. */
["SkupiniesConnection"]: {
	__typename: "SkupiniesConnection",
	/** A list of `Skupiny` objects. */
	nodes: Array<GraphQLTypes["Skupiny"]>,
	/** A list of edges which contains the `Skupiny` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["SkupiniesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Skupiny` you could get from the connection. */
	totalCount: number
};
	/** A `Skupiny` edge in the connection. */
["SkupiniesEdge"]: {
	__typename: "SkupiniesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Skupiny` at the end of the edge. */
	node: GraphQLTypes["Skupiny"]
};
	/** Methods to use when ordering `Skupiny`. */
["SkupiniesOrderBy"]: SkupiniesOrderBy;
	/** A condition to be used against `Skupiny` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["SkupinyCondition"]: {
		/** Checks for equality with the object’s `sId` field. */
	sId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `sName` field. */
	sName?: string,
	/** Checks for equality with the object’s `sDescription` field. */
	sDescription?: string,
	/** Checks for equality with the object’s `sColorRgb` field. */
	sColorRgb?: string,
	/** Checks for equality with the object’s `sColorText` field. */
	sColorText?: string,
	/** Checks for equality with the object’s `sLocation` field. */
	sLocation?: string,
	/** Checks for equality with the object’s `sVisible` field. */
	sVisible?: boolean
};
	/** A connection to a list of `UsersSkupiny` values. */
["UsersSkupiniesConnection"]: {
	__typename: "UsersSkupiniesConnection",
	/** A list of `UsersSkupiny` objects. */
	nodes: Array<GraphQLTypes["UsersSkupiny"]>,
	/** A list of edges which contains the `UsersSkupiny` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["UsersSkupiniesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `UsersSkupiny` you could get from the connection. */
	totalCount: number
};
	["UsersSkupiny"]: {
	__typename: "UsersSkupiny",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	usId: GraphQLTypes["BigInt"],
	usColor: string,
	usPlatbaMesic: GraphQLTypes["BigInt"],
	usPlatbaCtvrtrok: GraphQLTypes["BigInt"],
	usPlatbaPulrok: GraphQLTypes["BigInt"],
	usPopis: string
};
	/** A `UsersSkupiny` edge in the connection. */
["UsersSkupiniesEdge"]: {
	__typename: "UsersSkupiniesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `UsersSkupiny` at the end of the edge. */
	node: GraphQLTypes["UsersSkupiny"]
};
	/** Methods to use when ordering `UsersSkupiny`. */
["UsersSkupiniesOrderBy"]: UsersSkupiniesOrderBy;
	/** A condition to be used against `UsersSkupiny` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["UsersSkupinyCondition"]: {
		/** Checks for equality with the object’s `usId` field. */
	usId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `usColor` field. */
	usColor?: string,
	/** Checks for equality with the object’s `usPlatbaMesic` field. */
	usPlatbaMesic?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `usPlatbaCtvrtrok` field. */
	usPlatbaCtvrtrok?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `usPlatbaPulrok` field. */
	usPlatbaPulrok?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `usPopis` field. */
	usPopis?: string
};
	/** A connection to a list of `Video` values. */
["VideosConnection"]: {
	__typename: "VideosConnection",
	/** A list of `Video` objects. */
	nodes: Array<GraphQLTypes["Video"]>,
	/** A list of edges which contains the `Video` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["VideosEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Video` you could get from the connection. */
	totalCount: number
};
	["Video"]: {
	__typename: "Video",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	vId: GraphQLTypes["BigInt"],
	vUri: string,
	vTitle: string,
	vAuthor: string,
	vDescription: string,
	vPlaylist?: string,
	vCreatedAt: GraphQLTypes["Datetime"],
	vUpdatedAt: GraphQLTypes["Datetime"]
};
	/** A `Video` edge in the connection. */
["VideosEdge"]: {
	__typename: "VideosEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Video` at the end of the edge. */
	node: GraphQLTypes["Video"]
};
	/** Methods to use when ordering `Video`. */
["VideosOrderBy"]: VideosOrderBy;
	/** A condition to be used against `Video` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["VideoCondition"]: {
		/** Checks for equality with the object’s `vId` field. */
	vId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `vUri` field. */
	vUri?: string,
	/** Checks for equality with the object’s `vTitle` field. */
	vTitle?: string,
	/** Checks for equality with the object’s `vAuthor` field. */
	vAuthor?: string,
	/** Checks for equality with the object’s `vDescription` field. */
	vDescription?: string,
	/** Checks for equality with the object’s `vPlaylist` field. */
	vPlaylist?: string,
	/** Checks for equality with the object’s `vCreatedAt` field. */
	vCreatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `vUpdatedAt` field. */
	vUpdatedAt?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `VideoList` values. */
["VideoListsConnection"]: {
	__typename: "VideoListsConnection",
	/** A list of `VideoList` objects. */
	nodes: Array<GraphQLTypes["VideoList"]>,
	/** A list of edges which contains the `VideoList` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["VideoListsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `VideoList` you could get from the connection. */
	totalCount: number
};
	["VideoList"]: {
	__typename: "VideoList",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	vlId: GraphQLTypes["BigInt"],
	vlUrl: string,
	vlTitle: string,
	vlDescription: string,
	vlCount: GraphQLTypes["BigInt"],
	vlCreatedAt: GraphQLTypes["Datetime"],
	vlLastChecked?: GraphQLTypes["Datetime"]
};
	/** A `VideoList` edge in the connection. */
["VideoListsEdge"]: {
	__typename: "VideoListsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `VideoList` at the end of the edge. */
	node: GraphQLTypes["VideoList"]
};
	/** Methods to use when ordering `VideoList`. */
["VideoListsOrderBy"]: VideoListsOrderBy;
	/** A condition to be used against `VideoList` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoListCondition"]: {
		/** Checks for equality with the object’s `vlId` field. */
	vlId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `vlUrl` field. */
	vlUrl?: string,
	/** Checks for equality with the object’s `vlTitle` field. */
	vlTitle?: string,
	/** Checks for equality with the object’s `vlDescription` field. */
	vlDescription?: string,
	/** Checks for equality with the object’s `vlCount` field. */
	vlCount?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `vlCreatedAt` field. */
	vlCreatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `vlLastChecked` field. */
	vlLastChecked?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `VideoSource` values. */
["VideoSourcesConnection"]: {
	__typename: "VideoSourcesConnection",
	/** A list of `VideoSource` objects. */
	nodes: Array<GraphQLTypes["VideoSource"]>,
	/** A list of edges which contains the `VideoSource` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["VideoSourcesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `VideoSource` you could get from the connection. */
	totalCount: number
};
	["VideoSource"]: {
	__typename: "VideoSource",
	/** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
	nodeId: string,
	vsId: GraphQLTypes["BigInt"],
	vsUrl: string,
	vsTitle?: string,
	vsDescription?: string,
	vsCreatedAt: GraphQLTypes["Datetime"],
	vsLastChecked?: GraphQLTypes["Datetime"]
};
	/** A `VideoSource` edge in the connection. */
["VideoSourcesEdge"]: {
	__typename: "VideoSourcesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `VideoSource` at the end of the edge. */
	node: GraphQLTypes["VideoSource"]
};
	/** Methods to use when ordering `VideoSource`. */
["VideoSourcesOrderBy"]: VideoSourcesOrderBy;
	/** A condition to be used against `VideoSource` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["VideoSourceCondition"]: {
		/** Checks for equality with the object’s `vsId` field. */
	vsId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `vsUrl` field. */
	vsUrl?: string,
	/** Checks for equality with the object’s `vsTitle` field. */
	vsTitle?: string,
	/** Checks for equality with the object’s `vsDescription` field. */
	vsDescription?: string,
	/** Checks for equality with the object’s `vsCreatedAt` field. */
	vsCreatedAt?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `vsLastChecked` field. */
	vsLastChecked?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `BigInt` values. */
["CurrentCoupleIdsConnection"]: {
	__typename: "CurrentCoupleIdsConnection",
	/** A list of `BigInt` objects. */
	nodes?: Array<GraphQLTypes["BigInt"]>,
	/** A list of edges which contains the `BigInt` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["CurrentCoupleIdEdge"]>,
	/** The count of *all* `BigInt` you could get from the connection. */
	totalCount: number
};
	/** A `BigInt` edge in the connection. */
["CurrentCoupleIdEdge"]: {
	__typename: "CurrentCoupleIdEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `BigInt` at the end of the edge. */
	node?: GraphQLTypes["BigInt"]
};
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: {
	__typename: "Mutation",
	/** Creates a single `Akce`. */
	createAkce?: GraphQLTypes["CreateAkcePayload"],
	/** Creates a single `AkceItem`. */
	createAkceItem?: GraphQLTypes["CreateAkceItemPayload"],
	/** Creates a single `Aktuality`. */
	createAktuality?: GraphQLTypes["CreateAktualityPayload"],
	/** Creates a single `Dokumenty`. */
	createDokumenty?: GraphQLTypes["CreateDokumentyPayload"],
	/** Creates a single `GalerieDir`. */
	createGalerieDir?: GraphQLTypes["CreateGalerieDirPayload"],
	/** Creates a single `GalerieFoto`. */
	createGalerieFoto?: GraphQLTypes["CreateGalerieFotoPayload"],
	/** Creates a single `Nabidka`. */
	createNabidka?: GraphQLTypes["CreateNabidkaPayload"],
	/** Creates a single `NabidkaItem`. */
	createNabidkaItem?: GraphQLTypes["CreateNabidkaItemPayload"],
	/** Creates a single `Page`. */
	createPage?: GraphQLTypes["CreatePagePayload"],
	/** Creates a single `Parameter`. */
	createParameter?: GraphQLTypes["CreateParameterPayload"],
	/** Creates a single `Pary`. */
	createPary?: GraphQLTypes["CreateParyPayload"],
	/** Creates a single `ParyNavrh`. */
	createParyNavrh?: GraphQLTypes["CreateParyNavrhPayload"],
	/** Creates a single `Permission`. */
	createPermission?: GraphQLTypes["CreatePermissionPayload"],
	/** Creates a single `PlatbyCategory`. */
	createPlatbyCategory?: GraphQLTypes["CreatePlatbyCategoryPayload"],
	/** Creates a single `PlatbyCategoryGroup`. */
	createPlatbyCategoryGroup?: GraphQLTypes["CreatePlatbyCategoryGroupPayload"],
	/** Creates a single `PlatbyGroup`. */
	createPlatbyGroup?: GraphQLTypes["CreatePlatbyGroupPayload"],
	/** Creates a single `PlatbyGroupSkupina`. */
	createPlatbyGroupSkupina?: GraphQLTypes["CreatePlatbyGroupSkupinaPayload"],
	/** Creates a single `PlatbyItem`. */
	createPlatbyItem?: GraphQLTypes["CreatePlatbyItemPayload"],
	/** Creates a single `PlatbyRaw`. */
	createPlatbyRaw?: GraphQLTypes["CreatePlatbyRawPayload"],
	/** Creates a single `Rozpi`. */
	createRozpi?: GraphQLTypes["CreateRozpiPayload"],
	/** Creates a single `RozpisItem`. */
	createRozpisItem?: GraphQLTypes["CreateRozpisItemPayload"],
	/** Creates a single `Session`. */
	createSession?: GraphQLTypes["CreateSessionPayload"],
	/** Creates a single `Skupiny`. */
	createSkupiny?: GraphQLTypes["CreateSkupinyPayload"],
	/** Creates a single `Upozorneni`. */
	createUpozorneni?: GraphQLTypes["CreateUpozorneniPayload"],
	/** Creates a single `UpozorneniSkupiny`. */
	createUpozorneniSkupiny?: GraphQLTypes["CreateUpozorneniSkupinyPayload"],
	/** Creates a single `User`. */
	createUser?: GraphQLTypes["CreateUserPayload"],
	/** Creates a single `UsersSkupiny`. */
	createUsersSkupiny?: GraphQLTypes["CreateUsersSkupinyPayload"],
	/** Creates a single `Video`. */
	createVideo?: GraphQLTypes["CreateVideoPayload"],
	/** Creates a single `VideoList`. */
	createVideoList?: GraphQLTypes["CreateVideoListPayload"],
	/** Creates a single `VideoSource`. */
	createVideoSource?: GraphQLTypes["CreateVideoSourcePayload"],
	/** Updates a single `Akce` using its globally unique id and a patch. */
	updateAkceByNodeId?: GraphQLTypes["UpdateAkcePayload"],
	/** Updates a single `Akce` using a unique key and a patch. */
	updateAkce?: GraphQLTypes["UpdateAkcePayload"],
	/** Updates a single `AkceItem` using its globally unique id and a patch. */
	updateAkceItemByNodeId?: GraphQLTypes["UpdateAkceItemPayload"],
	/** Updates a single `AkceItem` using a unique key and a patch. */
	updateAkceItem?: GraphQLTypes["UpdateAkceItemPayload"],
	/** Updates a single `Aktuality` using its globally unique id and a patch. */
	updateAktualityByNodeId?: GraphQLTypes["UpdateAktualityPayload"],
	/** Updates a single `Aktuality` using a unique key and a patch. */
	updateAktuality?: GraphQLTypes["UpdateAktualityPayload"],
	/** Updates a single `Dokumenty` using its globally unique id and a patch. */
	updateDokumentyByNodeId?: GraphQLTypes["UpdateDokumentyPayload"],
	/** Updates a single `Dokumenty` using a unique key and a patch. */
	updateDokumenty?: GraphQLTypes["UpdateDokumentyPayload"],
	/** Updates a single `GalerieDir` using its globally unique id and a patch. */
	updateGalerieDirByNodeId?: GraphQLTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieDir` using a unique key and a patch. */
	updateGalerieDir?: GraphQLTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieFoto` using its globally unique id and a patch. */
	updateGalerieFotoByNodeId?: GraphQLTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `GalerieFoto` using a unique key and a patch. */
	updateGalerieFoto?: GraphQLTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `Nabidka` using its globally unique id and a patch. */
	updateNabidkaByNodeId?: GraphQLTypes["UpdateNabidkaPayload"],
	/** Updates a single `Nabidka` using a unique key and a patch. */
	updateNabidka?: GraphQLTypes["UpdateNabidkaPayload"],
	/** Updates a single `NabidkaItem` using its globally unique id and a patch. */
	updateNabidkaItemByNodeId?: GraphQLTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItem?: GraphQLTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `Page` using its globally unique id and a patch. */
	updatePageByNodeId?: GraphQLTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePage?: GraphQLTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePageByUrl?: GraphQLTypes["UpdatePagePayload"],
	/** Updates a single `Parameter` using its globally unique id and a patch. */
	updateParameterByNodeId?: GraphQLTypes["UpdateParameterPayload"],
	/** Updates a single `Parameter` using a unique key and a patch. */
	updateParameter?: GraphQLTypes["UpdateParameterPayload"],
	/** Updates a single `Pary` using its globally unique id and a patch. */
	updateParyByNodeId?: GraphQLTypes["UpdateParyPayload"],
	/** Updates a single `Pary` using a unique key and a patch. */
	updatePary?: GraphQLTypes["UpdateParyPayload"],
	/** Updates a single `ParyNavrh` using its globally unique id and a patch. */
	updateParyNavrhByNodeId?: GraphQLTypes["UpdateParyNavrhPayload"],
	/** Updates a single `ParyNavrh` using a unique key and a patch. */
	updateParyNavrh?: GraphQLTypes["UpdateParyNavrhPayload"],
	/** Updates a single `Permission` using its globally unique id and a patch. */
	updatePermissionByNodeId?: GraphQLTypes["UpdatePermissionPayload"],
	/** Updates a single `Permission` using a unique key and a patch. */
	updatePermission?: GraphQLTypes["UpdatePermissionPayload"],
	/** Updates a single `PlatbyCategory` using its globally unique id and a patch. */
	updatePlatbyCategoryByNodeId?: GraphQLTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategory` using a unique key and a patch. */
	updatePlatbyCategory?: GraphQLTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategoryGroup` using its globally unique id and a patch. */
	updatePlatbyCategoryGroupByNodeId?: GraphQLTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyCategoryGroup` using a unique key and a patch. */
	updatePlatbyCategoryGroup?: GraphQLTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyGroup` using its globally unique id and a patch. */
	updatePlatbyGroupByNodeId?: GraphQLTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroup` using a unique key and a patch. */
	updatePlatbyGroup?: GraphQLTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroupSkupina` using its globally unique id and a patch. */
	updatePlatbyGroupSkupinaByNodeId?: GraphQLTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyGroupSkupina` using a unique key and a patch. */
	updatePlatbyGroupSkupina?: GraphQLTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyItem` using its globally unique id and a patch. */
	updatePlatbyItemByNodeId?: GraphQLTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyItem` using a unique key and a patch. */
	updatePlatbyItem?: GraphQLTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyRaw` using its globally unique id and a patch. */
	updatePlatbyRawByNodeId?: GraphQLTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `PlatbyRaw` using a unique key and a patch. */
	updatePlatbyRaw?: GraphQLTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `Rozpi` using its globally unique id and a patch. */
	updateRozpiByNodeId?: GraphQLTypes["UpdateRozpiPayload"],
	/** Updates a single `Rozpi` using a unique key and a patch. */
	updateRozpi?: GraphQLTypes["UpdateRozpiPayload"],
	/** Updates a single `RozpisItem` using its globally unique id and a patch. */
	updateRozpisItemByNodeId?: GraphQLTypes["UpdateRozpisItemPayload"],
	/** Updates a single `RozpisItem` using a unique key and a patch. */
	updateRozpisItem?: GraphQLTypes["UpdateRozpisItemPayload"],
	/** Updates a single `Session` using its globally unique id and a patch. */
	updateSessionByNodeId?: GraphQLTypes["UpdateSessionPayload"],
	/** Updates a single `Session` using a unique key and a patch. */
	updateSession?: GraphQLTypes["UpdateSessionPayload"],
	/** Updates a single `Skupiny` using its globally unique id and a patch. */
	updateSkupinyByNodeId?: GraphQLTypes["UpdateSkupinyPayload"],
	/** Updates a single `Skupiny` using a unique key and a patch. */
	updateSkupiny?: GraphQLTypes["UpdateSkupinyPayload"],
	/** Updates a single `Upozorneni` using its globally unique id and a patch. */
	updateUpozorneniByNodeId?: GraphQLTypes["UpdateUpozorneniPayload"],
	/** Updates a single `Upozorneni` using a unique key and a patch. */
	updateUpozorneni?: GraphQLTypes["UpdateUpozorneniPayload"],
	/** Updates a single `UpozorneniSkupiny` using its globally unique id and a patch. */
	updateUpozorneniSkupinyByNodeId?: GraphQLTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `UpozorneniSkupiny` using a unique key and a patch. */
	updateUpozorneniSkupiny?: GraphQLTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `User` using its globally unique id and a patch. */
	updateUserByNodeId?: GraphQLTypes["UpdateUserPayload"],
	/** Updates a single `User` using a unique key and a patch. */
	updateUser?: GraphQLTypes["UpdateUserPayload"],
	/** Updates a single `UsersSkupiny` using its globally unique id and a patch. */
	updateUsersSkupinyByNodeId?: GraphQLTypes["UpdateUsersSkupinyPayload"],
	/** Updates a single `UsersSkupiny` using a unique key and a patch. */
	updateUsersSkupiny?: GraphQLTypes["UpdateUsersSkupinyPayload"],
	/** Updates a single `Video` using its globally unique id and a patch. */
	updateVideoByNodeId?: GraphQLTypes["UpdateVideoPayload"],
	/** Updates a single `Video` using a unique key and a patch. */
	updateVideo?: GraphQLTypes["UpdateVideoPayload"],
	/** Updates a single `VideoList` using its globally unique id and a patch. */
	updateVideoListByNodeId?: GraphQLTypes["UpdateVideoListPayload"],
	/** Updates a single `VideoList` using a unique key and a patch. */
	updateVideoList?: GraphQLTypes["UpdateVideoListPayload"],
	/** Updates a single `VideoSource` using its globally unique id and a patch. */
	updateVideoSourceByNodeId?: GraphQLTypes["UpdateVideoSourcePayload"],
	/** Updates a single `VideoSource` using a unique key and a patch. */
	updateVideoSource?: GraphQLTypes["UpdateVideoSourcePayload"],
	/** Deletes a single `Akce` using its globally unique id. */
	deleteAkceByNodeId?: GraphQLTypes["DeleteAkcePayload"],
	/** Deletes a single `Akce` using a unique key. */
	deleteAkce?: GraphQLTypes["DeleteAkcePayload"],
	/** Deletes a single `AkceItem` using its globally unique id. */
	deleteAkceItemByNodeId?: GraphQLTypes["DeleteAkceItemPayload"],
	/** Deletes a single `AkceItem` using a unique key. */
	deleteAkceItem?: GraphQLTypes["DeleteAkceItemPayload"],
	/** Deletes a single `Aktuality` using its globally unique id. */
	deleteAktualityByNodeId?: GraphQLTypes["DeleteAktualityPayload"],
	/** Deletes a single `Aktuality` using a unique key. */
	deleteAktuality?: GraphQLTypes["DeleteAktualityPayload"],
	/** Deletes a single `Dokumenty` using its globally unique id. */
	deleteDokumentyByNodeId?: GraphQLTypes["DeleteDokumentyPayload"],
	/** Deletes a single `Dokumenty` using a unique key. */
	deleteDokumenty?: GraphQLTypes["DeleteDokumentyPayload"],
	/** Deletes a single `GalerieDir` using its globally unique id. */
	deleteGalerieDirByNodeId?: GraphQLTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieDir` using a unique key. */
	deleteGalerieDir?: GraphQLTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieFoto` using its globally unique id. */
	deleteGalerieFotoByNodeId?: GraphQLTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `GalerieFoto` using a unique key. */
	deleteGalerieFoto?: GraphQLTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `Nabidka` using its globally unique id. */
	deleteNabidkaByNodeId?: GraphQLTypes["DeleteNabidkaPayload"],
	/** Deletes a single `Nabidka` using a unique key. */
	deleteNabidka?: GraphQLTypes["DeleteNabidkaPayload"],
	/** Deletes a single `NabidkaItem` using its globally unique id. */
	deleteNabidkaItemByNodeId?: GraphQLTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItem?: GraphQLTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `Parameter` using its globally unique id. */
	deleteParameterByNodeId?: GraphQLTypes["DeleteParameterPayload"],
	/** Deletes a single `Parameter` using a unique key. */
	deleteParameter?: GraphQLTypes["DeleteParameterPayload"],
	/** Deletes a single `Pary` using its globally unique id. */
	deleteParyByNodeId?: GraphQLTypes["DeleteParyPayload"],
	/** Deletes a single `Pary` using a unique key. */
	deletePary?: GraphQLTypes["DeleteParyPayload"],
	/** Deletes a single `ParyNavrh` using its globally unique id. */
	deleteParyNavrhByNodeId?: GraphQLTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `ParyNavrh` using a unique key. */
	deleteParyNavrh?: GraphQLTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `Permission` using its globally unique id. */
	deletePermissionByNodeId?: GraphQLTypes["DeletePermissionPayload"],
	/** Deletes a single `Permission` using a unique key. */
	deletePermission?: GraphQLTypes["DeletePermissionPayload"],
	/** Deletes a single `PlatbyCategory` using its globally unique id. */
	deletePlatbyCategoryByNodeId?: GraphQLTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategory` using a unique key. */
	deletePlatbyCategory?: GraphQLTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using its globally unique id. */
	deletePlatbyCategoryGroupByNodeId?: GraphQLTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using a unique key. */
	deletePlatbyCategoryGroup?: GraphQLTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyGroup` using its globally unique id. */
	deletePlatbyGroupByNodeId?: GraphQLTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroup` using a unique key. */
	deletePlatbyGroup?: GraphQLTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using its globally unique id. */
	deletePlatbyGroupSkupinaByNodeId?: GraphQLTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using a unique key. */
	deletePlatbyGroupSkupina?: GraphQLTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyItem` using its globally unique id. */
	deletePlatbyItemByNodeId?: GraphQLTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyItem` using a unique key. */
	deletePlatbyItem?: GraphQLTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyRaw` using its globally unique id. */
	deletePlatbyRawByNodeId?: GraphQLTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `PlatbyRaw` using a unique key. */
	deletePlatbyRaw?: GraphQLTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `Rozpi` using its globally unique id. */
	deleteRozpiByNodeId?: GraphQLTypes["DeleteRozpiPayload"],
	/** Deletes a single `Rozpi` using a unique key. */
	deleteRozpi?: GraphQLTypes["DeleteRozpiPayload"],
	/** Deletes a single `RozpisItem` using its globally unique id. */
	deleteRozpisItemByNodeId?: GraphQLTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `RozpisItem` using a unique key. */
	deleteRozpisItem?: GraphQLTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `Session` using its globally unique id. */
	deleteSessionByNodeId?: GraphQLTypes["DeleteSessionPayload"],
	/** Deletes a single `Session` using a unique key. */
	deleteSession?: GraphQLTypes["DeleteSessionPayload"],
	/** Deletes a single `Skupiny` using its globally unique id. */
	deleteSkupinyByNodeId?: GraphQLTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Skupiny` using a unique key. */
	deleteSkupiny?: GraphQLTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Upozorneni` using its globally unique id. */
	deleteUpozorneniByNodeId?: GraphQLTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `Upozorneni` using a unique key. */
	deleteUpozorneni?: GraphQLTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `UpozorneniSkupiny` using its globally unique id. */
	deleteUpozorneniSkupinyByNodeId?: GraphQLTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `UpozorneniSkupiny` using a unique key. */
	deleteUpozorneniSkupiny?: GraphQLTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `User` using its globally unique id. */
	deleteUserByNodeId?: GraphQLTypes["DeleteUserPayload"],
	/** Deletes a single `User` using a unique key. */
	deleteUser?: GraphQLTypes["DeleteUserPayload"],
	/** Deletes a single `UsersSkupiny` using its globally unique id. */
	deleteUsersSkupinyByNodeId?: GraphQLTypes["DeleteUsersSkupinyPayload"],
	/** Deletes a single `UsersSkupiny` using a unique key. */
	deleteUsersSkupiny?: GraphQLTypes["DeleteUsersSkupinyPayload"],
	/** Deletes a single `Video` using its globally unique id. */
	deleteVideoByNodeId?: GraphQLTypes["DeleteVideoPayload"],
	/** Deletes a single `Video` using a unique key. */
	deleteVideo?: GraphQLTypes["DeleteVideoPayload"],
	/** Deletes a single `VideoList` using its globally unique id. */
	deleteVideoListByNodeId?: GraphQLTypes["DeleteVideoListPayload"],
	/** Deletes a single `VideoList` using a unique key. */
	deleteVideoList?: GraphQLTypes["DeleteVideoListPayload"],
	/** Deletes a single `VideoSource` using its globally unique id. */
	deleteVideoSourceByNodeId?: GraphQLTypes["DeleteVideoSourcePayload"],
	/** Deletes a single `VideoSource` using a unique key. */
	deleteVideoSource?: GraphQLTypes["DeleteVideoSourcePayload"],
	login?: GraphQLTypes["LoginPayload"],
	logout?: GraphQLTypes["LogoutPayload"],
	uploadFile: GraphQLTypes["Upload"]
};
	/** The output of our create `Akce` mutation. */
["CreateAkcePayload"]: {
	__typename: "CreateAkcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Akce` that was created by this mutation. */
	akce?: GraphQLTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?: GraphQLTypes["AkcesEdge"]
};
	/** All input for the create `Akce` mutation. */
["CreateAkceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Akce` to be created by this mutation. */
	akce: GraphQLTypes["AkceInput"]
};
	/** An input for mutations affecting `Akce` */
["AkceInput"]: {
		aId?: GraphQLTypes["BigInt"],
	aJmeno: string,
	aKde: string,
	aInfo: string,
	aOd: GraphQLTypes["Date"],
	aDo: GraphQLTypes["Date"],
	aKapacita?: GraphQLTypes["BigInt"],
	aDokumenty: string,
	aTimestamp?: GraphQLTypes["Datetime"],
	aLock?: boolean,
	aVisible?: boolean
};
	/** The output of our create `AkceItem` mutation. */
["CreateAkceItemPayload"]: {
	__typename: "CreateAkceItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AkceItem` that was created by this mutation. */
	akceItem?: GraphQLTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?: GraphQLTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?: GraphQLTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?: GraphQLTypes["AkceItemsEdge"]
};
	/** All input for the create `AkceItem` mutation. */
["CreateAkceItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `AkceItem` to be created by this mutation. */
	akceItem: GraphQLTypes["AkceItemInput"]
};
	/** An input for mutations affecting `AkceItem` */
["AkceItemInput"]: {
		aiId?: GraphQLTypes["BigInt"],
	aiIdRodic: GraphQLTypes["BigInt"],
	aiUser: GraphQLTypes["BigInt"],
	aiRokNarozeni: number
};
	/** The output of our create `Aktuality` mutation. */
["CreateAktualityPayload"]: {
	__typename: "CreateAktualityPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Aktuality` that was created by this mutation. */
	aktuality?: GraphQLTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?: GraphQLTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?: GraphQLTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?: GraphQLTypes["AktualitiesEdge"]
};
	/** All input for the create `Aktuality` mutation. */
["CreateAktualityInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Aktuality` to be created by this mutation. */
	aktuality: GraphQLTypes["AktualityInput"]
};
	/** An input for mutations affecting `Aktuality` */
["AktualityInput"]: {
		atId?: GraphQLTypes["BigInt"],
	atKdo: GraphQLTypes["BigInt"],
	atKat: string,
	atJmeno: string,
	atText: string,
	atPreview: string,
	atFoto?: GraphQLTypes["BigInt"],
	atFotoMain?: GraphQLTypes["BigInt"],
	atTimestamp?: GraphQLTypes["Datetime"],
	atTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** The output of our create `Dokumenty` mutation. */
["CreateDokumentyPayload"]: {
	__typename: "CreateDokumentyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Dokumenty` that was created by this mutation. */
	dokumenty?: GraphQLTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?: GraphQLTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?: GraphQLTypes["DokumentiesEdge"]
};
	/** All input for the create `Dokumenty` mutation. */
["CreateDokumentyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Dokumenty` to be created by this mutation. */
	dokumenty: GraphQLTypes["DokumentyInput"]
};
	/** An input for mutations affecting `Dokumenty` */
["DokumentyInput"]: {
		dId?: GraphQLTypes["BigInt"],
	dPath: string,
	dName: string,
	dFilename: string,
	dKategorie: number,
	dKdo: GraphQLTypes["BigInt"],
	dTimestamp?: GraphQLTypes["Datetime"]
};
	/** The output of our create `GalerieDir` mutation. */
["CreateGalerieDirPayload"]: {
	__typename: "CreateGalerieDirPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieDir` that was created by this mutation. */
	galerieDir?: GraphQLTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?: GraphQLTypes["GalerieDirsEdge"]
};
	/** All input for the create `GalerieDir` mutation. */
["CreateGalerieDirInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `GalerieDir` to be created by this mutation. */
	galerieDir: GraphQLTypes["GalerieDirInput"]
};
	/** An input for mutations affecting `GalerieDir` */
["GalerieDirInput"]: {
		gdId?: GraphQLTypes["BigInt"],
	gdIdRodic: GraphQLTypes["BigInt"],
	gdName: string,
	gdLevel?: number,
	gdPath: string,
	gdHidden?: boolean
};
	/** The output of our create `GalerieFoto` mutation. */
["CreateGalerieFotoPayload"]: {
	__typename: "CreateGalerieFotoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieFoto` that was created by this mutation. */
	galerieFoto?: GraphQLTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?: GraphQLTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?: GraphQLTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?: GraphQLTypes["GalerieFotosEdge"]
};
	/** All input for the create `GalerieFoto` mutation. */
["CreateGalerieFotoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `GalerieFoto` to be created by this mutation. */
	galerieFoto: GraphQLTypes["GalerieFotoInput"]
};
	/** An input for mutations affecting `GalerieFoto` */
["GalerieFotoInput"]: {
		gfId?: GraphQLTypes["BigInt"],
	gfIdRodic: GraphQLTypes["BigInt"],
	gfName: string,
	gfPath: string,
	gfKdo: GraphQLTypes["BigInt"],
	gfTimestamp?: GraphQLTypes["Datetime"]
};
	/** The output of our create `Nabidka` mutation. */
["CreateNabidkaPayload"]: {
	__typename: "CreateNabidkaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Nabidka` that was created by this mutation. */
	nabidka?: GraphQLTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?: GraphQLTypes["NabidkasEdge"]
};
	/** All input for the create `Nabidka` mutation. */
["CreateNabidkaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Nabidka` to be created by this mutation. */
	nabidka: GraphQLTypes["NabidkaInput"]
};
	/** An input for mutations affecting `Nabidka` */
["NabidkaInput"]: {
		nId?: GraphQLTypes["BigInt"],
	nTrener: GraphQLTypes["BigInt"],
	nPocetHod?: number,
	nMaxPocetHod?: GraphQLTypes["BigInt"],
	nOd: GraphQLTypes["Date"],
	nDo: GraphQLTypes["Date"],
	nVisible?: boolean,
	nLock?: boolean,
	nTimestamp?: GraphQLTypes["Datetime"]
};
	/** The output of our create `NabidkaItem` mutation. */
["CreateNabidkaItemPayload"]: {
	__typename: "CreateNabidkaItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `NabidkaItem` that was created by this mutation. */
	nabidkaItem?: GraphQLTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?: GraphQLTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?: GraphQLTypes["NabidkaItemsEdge"]
};
	/** All input for the create `NabidkaItem` mutation. */
["CreateNabidkaItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `NabidkaItem` to be created by this mutation. */
	nabidkaItem: GraphQLTypes["NabidkaItemInput"]
};
	/** An input for mutations affecting `NabidkaItem` */
["NabidkaItemInput"]: {
		niId?: GraphQLTypes["BigInt"],
	niIdRodic: GraphQLTypes["BigInt"],
	niPartner: GraphQLTypes["BigInt"],
	niPocetHod?: number,
	niLock?: boolean
};
	/** The output of our create `Page` mutation. */
["CreatePagePayload"]: {
	__typename: "CreatePagePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Page` that was created by this mutation. */
	page?: GraphQLTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Page`. May be used by Relay 1. */
	pageEdge?: GraphQLTypes["PagesEdge"]
};
	/** All input for the create `Page` mutation. */
["CreatePageInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Page` to be created by this mutation. */
	page: GraphQLTypes["PageInput"]
};
	/** An input for mutations affecting `Page` */
["PageInput"]: {
		id?: number,
	url: string,
	content: GraphQLTypes["JSON"],
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	title?: string
};
	/** The output of our create `Parameter` mutation. */
["CreateParameterPayload"]: {
	__typename: "CreateParameterPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Parameter` that was created by this mutation. */
	parameter?: GraphQLTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?: GraphQLTypes["ParametersEdge"]
};
	/** All input for the create `Parameter` mutation. */
["CreateParameterInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Parameter` to be created by this mutation. */
	parameter: GraphQLTypes["ParameterInput"]
};
	/** An input for mutations affecting `Parameter` */
["ParameterInput"]: {
		paName: string,
	paValue: string
};
	/** The output of our create `Pary` mutation. */
["CreateParyPayload"]: {
	__typename: "CreateParyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Pary` that was created by this mutation. */
	pary?: GraphQLTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?: GraphQLTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?: GraphQLTypes["PariesEdge"]
};
	/** All input for the create `Pary` mutation. */
["CreateParyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Pary` to be created by this mutation. */
	pary: GraphQLTypes["ParyInput"]
};
	/** An input for mutations affecting `Pary` */
["ParyInput"]: {
		pId?: GraphQLTypes["BigInt"],
	pIdPartner: GraphQLTypes["BigInt"],
	pIdPartnerka?: GraphQLTypes["BigInt"],
	pSttTrida?: GraphQLTypes["ParyPSttTrida"],
	pSttBody?: number,
	pSttFinale?: boolean,
	pLatTrida?: GraphQLTypes["ParyPLatTrida"],
	pLatBody?: number,
	pLatFinale?: boolean,
	pHodnoceni?: number,
	pArchiv?: boolean,
	pTimestampAdd?: GraphQLTypes["Datetime"],
	pTimestampArchive?: GraphQLTypes["Datetime"]
};
	/** The output of our create `ParyNavrh` mutation. */
["CreateParyNavrhPayload"]: {
	__typename: "CreateParyNavrhPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `ParyNavrh` that was created by this mutation. */
	paryNavrh?: GraphQLTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?: GraphQLTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?: GraphQLTypes["ParyNavrhsEdge"]
};
	/** All input for the create `ParyNavrh` mutation. */
["CreateParyNavrhInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `ParyNavrh` to be created by this mutation. */
	paryNavrh: GraphQLTypes["ParyNavrhInput"]
};
	/** An input for mutations affecting `ParyNavrh` */
["ParyNavrhInput"]: {
		pnId?: GraphQLTypes["BigInt"],
	pnNavrhl: GraphQLTypes["BigInt"],
	pnPartner: GraphQLTypes["BigInt"],
	pnPartnerka: GraphQLTypes["BigInt"]
};
	/** The output of our create `Permission` mutation. */
["CreatePermissionPayload"]: {
	__typename: "CreatePermissionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Permission` that was created by this mutation. */
	permission?: GraphQLTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?: GraphQLTypes["PermissionsEdge"]
};
	/** All input for the create `Permission` mutation. */
["CreatePermissionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Permission` to be created by this mutation. */
	permission: GraphQLTypes["PermissionInput"]
};
	/** An input for mutations affecting `Permission` */
["PermissionInput"]: {
		peId?: GraphQLTypes["BigInt"],
	peName: string,
	peDescription: string,
	peAkce: number,
	peAktuality: number,
	peAnkety: number,
	peDokumenty: number,
	peGalerie: number,
	peInzerce: number,
	peKonzole: number,
	peNabidka: number,
	peNastenka: number,
	peNovinky: number,
	pePary: number,
	pePlatby: number,
	pePermissions: number,
	peRozpis: number,
	peSkupiny: number,
	peUsers: number,
	peMain: number
};
	/** The output of our create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryPayload"]: {
	__typename: "CreatePlatbyCategoryPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategory` that was created by this mutation. */
	platbyCategory?: GraphQLTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?: GraphQLTypes["PlatbyCategoriesEdge"]
};
	/** All input for the create `PlatbyCategory` mutation. */
["CreatePlatbyCategoryInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyCategory` to be created by this mutation. */
	platbyCategory: GraphQLTypes["PlatbyCategoryInput"]
};
	/** An input for mutations affecting `PlatbyCategory` */
["PlatbyCategoryInput"]: {
		pcId?: GraphQLTypes["BigInt"],
	pcName: string,
	pcSymbol: GraphQLTypes["BigInt"],
	pcAmount: GraphQLTypes["BigFloat"],
	pcDateDue: GraphQLTypes["Date"],
	pcValidFrom: GraphQLTypes["Date"],
	pcValidTo: GraphQLTypes["Date"],
	pcUseBase?: boolean,
	pcUsePrefix?: boolean,
	pcArchive?: boolean,
	pcVisible?: boolean
};
	/** The output of our create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupPayload"]: {
	__typename: "CreatePlatbyCategoryGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategoryGroup` that was created by this mutation. */
	platbyCategoryGroup?: GraphQLTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?: GraphQLTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the create `PlatbyCategoryGroup` mutation. */
["CreatePlatbyCategoryGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyCategoryGroup` to be created by this mutation. */
	platbyCategoryGroup: GraphQLTypes["PlatbyCategoryGroupInput"]
};
	/** An input for mutations affecting `PlatbyCategoryGroup` */
["PlatbyCategoryGroupInput"]: {
		pcgId?: GraphQLTypes["BigInt"],
	pcgIdGroup: GraphQLTypes["BigInt"],
	pcgIdCategory: GraphQLTypes["BigInt"]
};
	/** The output of our create `PlatbyGroup` mutation. */
["CreatePlatbyGroupPayload"]: {
	__typename: "CreatePlatbyGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroup` that was created by this mutation. */
	platbyGroup?: GraphQLTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?: GraphQLTypes["PlatbyGroupsEdge"]
};
	/** All input for the create `PlatbyGroup` mutation. */
["CreatePlatbyGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyGroup` to be created by this mutation. */
	platbyGroup: GraphQLTypes["PlatbyGroupInput"]
};
	/** An input for mutations affecting `PlatbyGroup` */
["PlatbyGroupInput"]: {
		pgId?: GraphQLTypes["BigInt"],
	pgType?: GraphQLTypes["BigFloat"],
	pgName: string,
	pgDescription: string,
	pgBase?: GraphQLTypes["BigInt"]
};
	/** The output of our create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaPayload"]: {
	__typename: "CreatePlatbyGroupSkupinaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroupSkupina` that was created by this mutation. */
	platbyGroupSkupina?: GraphQLTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?: GraphQLTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?: GraphQLTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the create `PlatbyGroupSkupina` mutation. */
["CreatePlatbyGroupSkupinaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyGroupSkupina` to be created by this mutation. */
	platbyGroupSkupina: GraphQLTypes["PlatbyGroupSkupinaInput"]
};
	/** An input for mutations affecting `PlatbyGroupSkupina` */
["PlatbyGroupSkupinaInput"]: {
		pgsId?: GraphQLTypes["BigInt"],
	pgsIdSkupina: GraphQLTypes["BigInt"],
	pgsIdGroup: GraphQLTypes["BigInt"]
};
	/** The output of our create `PlatbyItem` mutation. */
["CreatePlatbyItemPayload"]: {
	__typename: "CreatePlatbyItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyItem` that was created by this mutation. */
	platbyItem?: GraphQLTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?: GraphQLTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?: GraphQLTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?: GraphQLTypes["PlatbyItemsEdge"]
};
	/** All input for the create `PlatbyItem` mutation. */
["CreatePlatbyItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyItem` to be created by this mutation. */
	platbyItem: GraphQLTypes["PlatbyItemInput"]
};
	/** An input for mutations affecting `PlatbyItem` */
["PlatbyItemInput"]: {
		piId?: GraphQLTypes["BigInt"],
	piIdUser?: GraphQLTypes["BigInt"],
	piIdCategory: GraphQLTypes["BigInt"],
	piIdRaw?: GraphQLTypes["BigInt"],
	piAmount: GraphQLTypes["BigFloat"],
	piDate: GraphQLTypes["Date"],
	piPrefix?: number
};
	/** The output of our create `PlatbyRaw` mutation. */
["CreatePlatbyRawPayload"]: {
	__typename: "CreatePlatbyRawPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyRaw` that was created by this mutation. */
	platbyRaw?: GraphQLTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?: GraphQLTypes["PlatbyRawsEdge"]
};
	/** All input for the create `PlatbyRaw` mutation. */
["CreatePlatbyRawInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `PlatbyRaw` to be created by this mutation. */
	platbyRaw: GraphQLTypes["PlatbyRawInput"]
};
	/** An input for mutations affecting `PlatbyRaw` */
["PlatbyRawInput"]: {
		prId?: GraphQLTypes["BigInt"],
	prRaw: string,
	prHash: string,
	prSorted?: boolean,
	prDiscarded?: boolean
};
	/** The output of our create `Rozpi` mutation. */
["CreateRozpiPayload"]: {
	__typename: "CreateRozpiPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Rozpi` that was created by this mutation. */
	rozpi?: GraphQLTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?: GraphQLTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?: GraphQLTypes["RozpisEdge"]
};
	/** All input for the create `Rozpi` mutation. */
["CreateRozpiInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Rozpi` to be created by this mutation. */
	rozpi: GraphQLTypes["RozpiInput"]
};
	/** An input for mutations affecting `Rozpi` */
["RozpiInput"]: {
		rId?: GraphQLTypes["BigInt"],
	rTrener: GraphQLTypes["BigInt"],
	rKde: string,
	rDatum: GraphQLTypes["Date"],
	rVisible?: boolean,
	rLock?: boolean,
	rTimestamp?: GraphQLTypes["Datetime"]
};
	/** The output of our create `RozpisItem` mutation. */
["CreateRozpisItemPayload"]: {
	__typename: "CreateRozpisItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RozpisItem` that was created by this mutation. */
	rozpisItem?: GraphQLTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?: GraphQLTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?: GraphQLTypes["RozpisItemsEdge"]
};
	/** All input for the create `RozpisItem` mutation. */
["CreateRozpisItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `RozpisItem` to be created by this mutation. */
	rozpisItem: GraphQLTypes["RozpisItemInput"]
};
	/** An input for mutations affecting `RozpisItem` */
["RozpisItemInput"]: {
		riId?: GraphQLTypes["BigInt"],
	riIdRodic: GraphQLTypes["BigInt"],
	riPartner?: GraphQLTypes["BigInt"],
	riOd: GraphQLTypes["Time"],
	riDo: GraphQLTypes["Time"],
	riLock?: boolean
};
	/** The output of our create `Session` mutation. */
["CreateSessionPayload"]: {
	__typename: "CreateSessionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Session` that was created by this mutation. */
	session?: GraphQLTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?: GraphQLTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?: GraphQLTypes["SessionsEdge"]
};
	/** All input for the create `Session` mutation. */
["CreateSessionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Session` to be created by this mutation. */
	session: GraphQLTypes["SessionInput"]
};
	/** An input for mutations affecting `Session` */
["SessionInput"]: {
		ssId: string,
	ssData: string,
	ssUpdatedAt?: GraphQLTypes["Datetime"],
	ssLifetime: GraphQLTypes["BigInt"],
	ssUser?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Skupiny` mutation. */
["CreateSkupinyPayload"]: {
	__typename: "CreateSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Skupiny` that was created by this mutation. */
	skupiny?: GraphQLTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?: GraphQLTypes["SkupiniesEdge"]
};
	/** All input for the create `Skupiny` mutation. */
["CreateSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Skupiny` to be created by this mutation. */
	skupiny: GraphQLTypes["SkupinyInput"]
};
	/** An input for mutations affecting `Skupiny` */
["SkupinyInput"]: {
		sId?: GraphQLTypes["BigInt"],
	sName: string,
	sDescription: string,
	sColorRgb: string,
	sColorText: string,
	sLocation?: string,
	sVisible?: boolean
};
	/** The output of our create `Upozorneni` mutation. */
["CreateUpozorneniPayload"]: {
	__typename: "CreateUpozorneniPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Upozorneni` that was created by this mutation. */
	upozorneni?: GraphQLTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?: GraphQLTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?: GraphQLTypes["UpozornenisEdge"]
};
	/** All input for the create `Upozorneni` mutation. */
["CreateUpozorneniInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Upozorneni` to be created by this mutation. */
	upozorneni: GraphQLTypes["UpozorneniInput"]
};
	/** An input for mutations affecting `Upozorneni` */
["UpozorneniInput"]: {
		upId?: GraphQLTypes["BigInt"],
	upKdo: GraphQLTypes["BigInt"],
	upNadpis: string,
	upText: string,
	upBarvy?: GraphQLTypes["BigInt"],
	upLock?: boolean,
	upTimestamp?: GraphQLTypes["Datetime"],
	upTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** The output of our create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyPayload"]: {
	__typename: "CreateUpozorneniSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UpozorneniSkupiny` that was created by this mutation. */
	upozorneniSkupiny?: GraphQLTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?: GraphQLTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?: GraphQLTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the create `UpozorneniSkupiny` mutation. */
["CreateUpozorneniSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `UpozorneniSkupiny` to be created by this mutation. */
	upozorneniSkupiny: GraphQLTypes["UpozorneniSkupinyInput"]
};
	/** An input for mutations affecting `UpozorneniSkupiny` */
["UpozorneniSkupinyInput"]: {
		upsId?: GraphQLTypes["BigInt"],
	upsIdRodic: GraphQLTypes["BigInt"],
	upsIdSkupina: GraphQLTypes["BigInt"],
	upsColor: string,
	upsPopis: string
};
	/** The output of our create `User` mutation. */
["CreateUserPayload"]: {
	__typename: "CreateUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `User` that was created by this mutation. */
	user?: GraphQLTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?: GraphQLTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?: GraphQLTypes["UsersEdge"]
};
	/** All input for the create `User` mutation. */
["CreateUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `User` to be created by this mutation. */
	user: GraphQLTypes["UserInput"]
};
	/** An input for mutations affecting `User` */
["UserInput"]: {
		uId?: GraphQLTypes["BigInt"],
	uLogin: string,
	uPass: string,
	uJmeno: string,
	uPrijmeni: string,
	uPohlavi: string,
	uEmail: string,
	uTelefon: string,
	uNarozeni: GraphQLTypes["Date"],
	uRodneCislo?: string,
	uPoznamky?: string,
	uTimestamp?: GraphQLTypes["Datetime"],
	uLevel?: number,
	uGroup: GraphQLTypes["BigInt"],
	uSkupina?: GraphQLTypes["BigInt"],
	uDancer?: boolean,
	uBan?: boolean,
	uLock?: boolean,
	uConfirmed?: boolean,
	uSystem?: boolean,
	uStreet: string,
	uConscriptionNumber?: string,
	uOrientationNumber?: string,
	uDistrict?: string,
	uCity: string,
	uPostalCode: string,
	uNationality: string,
	uMemberSince?: GraphQLTypes["Datetime"],
	uMemberUntil?: GraphQLTypes["Datetime"],
	uCreatedAt?: GraphQLTypes["Datetime"],
	uTeacher?: boolean,
	uGdprSignedAt?: GraphQLTypes["Datetime"]
};
	/** The output of our create `UsersSkupiny` mutation. */
["CreateUsersSkupinyPayload"]: {
	__typename: "CreateUsersSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UsersSkupiny` that was created by this mutation. */
	usersSkupiny?: GraphQLTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?: GraphQLTypes["UsersSkupiniesEdge"]
};
	/** All input for the create `UsersSkupiny` mutation. */
["CreateUsersSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `UsersSkupiny` to be created by this mutation. */
	usersSkupiny: GraphQLTypes["UsersSkupinyInput"]
};
	/** An input for mutations affecting `UsersSkupiny` */
["UsersSkupinyInput"]: {
		usId?: GraphQLTypes["BigInt"],
	usColor?: string,
	usPlatbaMesic?: GraphQLTypes["BigInt"],
	usPlatbaCtvrtrok?: GraphQLTypes["BigInt"],
	usPlatbaPulrok?: GraphQLTypes["BigInt"],
	usPopis: string
};
	/** The output of our create `Video` mutation. */
["CreateVideoPayload"]: {
	__typename: "CreateVideoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Video` that was created by this mutation. */
	video?: GraphQLTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?: GraphQLTypes["VideosEdge"]
};
	/** All input for the create `Video` mutation. */
["CreateVideoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Video` to be created by this mutation. */
	video: GraphQLTypes["VideoInput"]
};
	/** An input for mutations affecting `Video` */
["VideoInput"]: {
		vId?: GraphQLTypes["BigInt"],
	vUri: string,
	vTitle: string,
	vAuthor: string,
	vDescription: string,
	vPlaylist?: string,
	vCreatedAt: GraphQLTypes["Datetime"],
	vUpdatedAt?: GraphQLTypes["Datetime"]
};
	/** The output of our create `VideoList` mutation. */
["CreateVideoListPayload"]: {
	__typename: "CreateVideoListPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoList` that was created by this mutation. */
	videoList?: GraphQLTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?: GraphQLTypes["VideoListsEdge"]
};
	/** All input for the create `VideoList` mutation. */
["CreateVideoListInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `VideoList` to be created by this mutation. */
	videoList: GraphQLTypes["VideoListInput"]
};
	/** An input for mutations affecting `VideoList` */
["VideoListInput"]: {
		vlId?: GraphQLTypes["BigInt"],
	vlUrl: string,
	vlTitle: string,
	vlDescription: string,
	vlCount: GraphQLTypes["BigInt"],
	vlCreatedAt: GraphQLTypes["Datetime"],
	vlLastChecked?: GraphQLTypes["Datetime"]
};
	/** The output of our create `VideoSource` mutation. */
["CreateVideoSourcePayload"]: {
	__typename: "CreateVideoSourcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoSource` that was created by this mutation. */
	videoSource?: GraphQLTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?: GraphQLTypes["VideoSourcesEdge"]
};
	/** All input for the create `VideoSource` mutation. */
["CreateVideoSourceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `VideoSource` to be created by this mutation. */
	videoSource: GraphQLTypes["VideoSourceInput"]
};
	/** An input for mutations affecting `VideoSource` */
["VideoSourceInput"]: {
		vsId?: GraphQLTypes["BigInt"],
	vsUrl: string,
	vsTitle?: string,
	vsDescription?: string,
	vsCreatedAt?: GraphQLTypes["Datetime"],
	vsLastChecked?: GraphQLTypes["Datetime"]
};
	/** The output of our update `Akce` mutation. */
["UpdateAkcePayload"]: {
	__typename: "UpdateAkcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Akce` that was updated by this mutation. */
	akce?: GraphQLTypes["Akce"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?: GraphQLTypes["AkcesEdge"]
};
	/** All input for the `updateAkceByNodeId` mutation. */
["UpdateAkceByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Akce` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Akce` being updated. */
	patch: GraphQLTypes["AkcePatch"]
};
	/** Represents an update to a `Akce`. Fields that are set will be updated. */
["AkcePatch"]: {
		aId?: GraphQLTypes["BigInt"],
	aJmeno?: string,
	aKde?: string,
	aInfo?: string,
	aOd?: GraphQLTypes["Date"],
	aDo?: GraphQLTypes["Date"],
	aKapacita?: GraphQLTypes["BigInt"],
	aDokumenty?: string,
	aTimestamp?: GraphQLTypes["Datetime"],
	aLock?: boolean,
	aVisible?: boolean
};
	/** All input for the `updateAkce` mutation. */
["UpdateAkceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Akce` being updated. */
	patch: GraphQLTypes["AkcePatch"],
	aId: GraphQLTypes["BigInt"]
};
	/** The output of our update `AkceItem` mutation. */
["UpdateAkceItemPayload"]: {
	__typename: "UpdateAkceItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AkceItem` that was updated by this mutation. */
	akceItem?: GraphQLTypes["AkceItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?: GraphQLTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?: GraphQLTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?: GraphQLTypes["AkceItemsEdge"]
};
	/** All input for the `updateAkceItemByNodeId` mutation. */
["UpdateAkceItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `AkceItem` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `AkceItem` being updated. */
	patch: GraphQLTypes["AkceItemPatch"]
};
	/** Represents an update to a `AkceItem`. Fields that are set will be updated. */
["AkceItemPatch"]: {
		aiId?: GraphQLTypes["BigInt"],
	aiIdRodic?: GraphQLTypes["BigInt"],
	aiUser?: GraphQLTypes["BigInt"],
	aiRokNarozeni?: number
};
	/** All input for the `updateAkceItem` mutation. */
["UpdateAkceItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `AkceItem` being updated. */
	patch: GraphQLTypes["AkceItemPatch"],
	aiId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Aktuality` mutation. */
["UpdateAktualityPayload"]: {
	__typename: "UpdateAktualityPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Aktuality` that was updated by this mutation. */
	aktuality?: GraphQLTypes["Aktuality"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?: GraphQLTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?: GraphQLTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?: GraphQLTypes["AktualitiesEdge"]
};
	/** All input for the `updateAktualityByNodeId` mutation. */
["UpdateAktualityByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Aktuality` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch: GraphQLTypes["AktualityPatch"]
};
	/** Represents an update to a `Aktuality`. Fields that are set will be updated. */
["AktualityPatch"]: {
		atId?: GraphQLTypes["BigInt"],
	atKdo?: GraphQLTypes["BigInt"],
	atKat?: string,
	atJmeno?: string,
	atText?: string,
	atPreview?: string,
	atFoto?: GraphQLTypes["BigInt"],
	atFotoMain?: GraphQLTypes["BigInt"],
	atTimestamp?: GraphQLTypes["Datetime"],
	atTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch: GraphQLTypes["AktualityPatch"],
	atId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Dokumenty` mutation. */
["UpdateDokumentyPayload"]: {
	__typename: "UpdateDokumentyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Dokumenty` that was updated by this mutation. */
	dokumenty?: GraphQLTypes["Dokumenty"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?: GraphQLTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?: GraphQLTypes["DokumentiesEdge"]
};
	/** All input for the `updateDokumentyByNodeId` mutation. */
["UpdateDokumentyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Dokumenty` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch: GraphQLTypes["DokumentyPatch"]
};
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: {
		dId?: GraphQLTypes["BigInt"],
	dPath?: string,
	dName?: string,
	dFilename?: string,
	dKategorie?: number,
	dKdo?: GraphQLTypes["BigInt"],
	dTimestamp?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch: GraphQLTypes["DokumentyPatch"],
	dId: GraphQLTypes["BigInt"]
};
	/** The output of our update `GalerieDir` mutation. */
["UpdateGalerieDirPayload"]: {
	__typename: "UpdateGalerieDirPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieDir` that was updated by this mutation. */
	galerieDir?: GraphQLTypes["GalerieDir"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?: GraphQLTypes["GalerieDirsEdge"]
};
	/** All input for the `updateGalerieDirByNodeId` mutation. */
["UpdateGalerieDirByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `GalerieDir` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch: GraphQLTypes["GalerieDirPatch"]
};
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: {
		gdId?: GraphQLTypes["BigInt"],
	gdIdRodic?: GraphQLTypes["BigInt"],
	gdName?: string,
	gdLevel?: number,
	gdPath?: string,
	gdHidden?: boolean
};
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch: GraphQLTypes["GalerieDirPatch"],
	gdId: GraphQLTypes["BigInt"]
};
	/** The output of our update `GalerieFoto` mutation. */
["UpdateGalerieFotoPayload"]: {
	__typename: "UpdateGalerieFotoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieFoto` that was updated by this mutation. */
	galerieFoto?: GraphQLTypes["GalerieFoto"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?: GraphQLTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?: GraphQLTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?: GraphQLTypes["GalerieFotosEdge"]
};
	/** All input for the `updateGalerieFotoByNodeId` mutation. */
["UpdateGalerieFotoByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `GalerieFoto` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch: GraphQLTypes["GalerieFotoPatch"]
};
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: {
		gfId?: GraphQLTypes["BigInt"],
	gfIdRodic?: GraphQLTypes["BigInt"],
	gfName?: string,
	gfPath?: string,
	gfKdo?: GraphQLTypes["BigInt"],
	gfTimestamp?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch: GraphQLTypes["GalerieFotoPatch"],
	gfId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Nabidka` mutation. */
["UpdateNabidkaPayload"]: {
	__typename: "UpdateNabidkaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Nabidka` that was updated by this mutation. */
	nabidka?: GraphQLTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?: GraphQLTypes["NabidkasEdge"]
};
	/** All input for the `updateNabidkaByNodeId` mutation. */
["UpdateNabidkaByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Nabidka` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch: GraphQLTypes["NabidkaPatch"]
};
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: {
		nId?: GraphQLTypes["BigInt"],
	nTrener?: GraphQLTypes["BigInt"],
	nPocetHod?: number,
	nMaxPocetHod?: GraphQLTypes["BigInt"],
	nOd?: GraphQLTypes["Date"],
	nDo?: GraphQLTypes["Date"],
	nVisible?: boolean,
	nLock?: boolean,
	nTimestamp?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch: GraphQLTypes["NabidkaPatch"],
	nId: GraphQLTypes["BigInt"]
};
	/** The output of our update `NabidkaItem` mutation. */
["UpdateNabidkaItemPayload"]: {
	__typename: "UpdateNabidkaItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `NabidkaItem` that was updated by this mutation. */
	nabidkaItem?: GraphQLTypes["NabidkaItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?: GraphQLTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?: GraphQLTypes["NabidkaItemsEdge"]
};
	/** All input for the `updateNabidkaItemByNodeId` mutation. */
["UpdateNabidkaItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `NabidkaItem` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch: GraphQLTypes["NabidkaItemPatch"]
};
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: {
		niId?: GraphQLTypes["BigInt"],
	niIdRodic?: GraphQLTypes["BigInt"],
	niPartner?: GraphQLTypes["BigInt"],
	niPocetHod?: number,
	niLock?: boolean
};
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch: GraphQLTypes["NabidkaItemPatch"],
	niId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Page` mutation. */
["UpdatePagePayload"]: {
	__typename: "UpdatePagePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Page` that was updated by this mutation. */
	page?: GraphQLTypes["Page"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Page`. May be used by Relay 1. */
	pageEdge?: GraphQLTypes["PagesEdge"]
};
	/** All input for the `updatePageByNodeId` mutation. */
["UpdatePageByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Page` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch: GraphQLTypes["PagePatch"]
};
	/** Represents an update to a `Page`. Fields that are set will be updated. */
["PagePatch"]: {
		id?: number,
	url?: string,
	content?: GraphQLTypes["JSON"],
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	title?: string
};
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch: GraphQLTypes["PagePatch"],
	id: number
};
	/** All input for the `updatePageByUrl` mutation. */
["UpdatePageByUrlInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch: GraphQLTypes["PagePatch"],
	url: string
};
	/** The output of our update `Parameter` mutation. */
["UpdateParameterPayload"]: {
	__typename: "UpdateParameterPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Parameter` that was updated by this mutation. */
	parameter?: GraphQLTypes["Parameter"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?: GraphQLTypes["ParametersEdge"]
};
	/** All input for the `updateParameterByNodeId` mutation. */
["UpdateParameterByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Parameter` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch: GraphQLTypes["ParameterPatch"]
};
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: {
		paName?: string,
	paValue?: string
};
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch: GraphQLTypes["ParameterPatch"],
	paName: string
};
	/** The output of our update `Pary` mutation. */
["UpdateParyPayload"]: {
	__typename: "UpdateParyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Pary` that was updated by this mutation. */
	pary?: GraphQLTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?: GraphQLTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?: GraphQLTypes["PariesEdge"]
};
	/** All input for the `updateParyByNodeId` mutation. */
["UpdateParyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Pary` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Pary` being updated. */
	patch: GraphQLTypes["ParyPatch"]
};
	/** Represents an update to a `Pary`. Fields that are set will be updated. */
["ParyPatch"]: {
		pId?: GraphQLTypes["BigInt"],
	pIdPartner?: GraphQLTypes["BigInt"],
	pIdPartnerka?: GraphQLTypes["BigInt"],
	pSttTrida?: GraphQLTypes["ParyPSttTrida"],
	pSttBody?: number,
	pSttFinale?: boolean,
	pLatTrida?: GraphQLTypes["ParyPLatTrida"],
	pLatBody?: number,
	pLatFinale?: boolean,
	pHodnoceni?: number,
	pArchiv?: boolean,
	pTimestampAdd?: GraphQLTypes["Datetime"],
	pTimestampArchive?: GraphQLTypes["Datetime"]
};
	/** All input for the `updatePary` mutation. */
["UpdateParyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Pary` being updated. */
	patch: GraphQLTypes["ParyPatch"],
	pId: GraphQLTypes["BigInt"]
};
	/** The output of our update `ParyNavrh` mutation. */
["UpdateParyNavrhPayload"]: {
	__typename: "UpdateParyNavrhPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `ParyNavrh` that was updated by this mutation. */
	paryNavrh?: GraphQLTypes["ParyNavrh"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?: GraphQLTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?: GraphQLTypes["ParyNavrhsEdge"]
};
	/** All input for the `updateParyNavrhByNodeId` mutation. */
["UpdateParyNavrhByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `ParyNavrh` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch: GraphQLTypes["ParyNavrhPatch"]
};
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: {
		pnId?: GraphQLTypes["BigInt"],
	pnNavrhl?: GraphQLTypes["BigInt"],
	pnPartner?: GraphQLTypes["BigInt"],
	pnPartnerka?: GraphQLTypes["BigInt"]
};
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch: GraphQLTypes["ParyNavrhPatch"],
	pnId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Permission` mutation. */
["UpdatePermissionPayload"]: {
	__typename: "UpdatePermissionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Permission` that was updated by this mutation. */
	permission?: GraphQLTypes["Permission"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?: GraphQLTypes["PermissionsEdge"]
};
	/** All input for the `updatePermissionByNodeId` mutation. */
["UpdatePermissionByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Permission` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch: GraphQLTypes["PermissionPatch"]
};
	/** Represents an update to a `Permission`. Fields that are set will be updated. */
["PermissionPatch"]: {
		peId?: GraphQLTypes["BigInt"],
	peName?: string,
	peDescription?: string,
	peAkce?: number,
	peAktuality?: number,
	peAnkety?: number,
	peDokumenty?: number,
	peGalerie?: number,
	peInzerce?: number,
	peKonzole?: number,
	peNabidka?: number,
	peNastenka?: number,
	peNovinky?: number,
	pePary?: number,
	pePlatby?: number,
	pePermissions?: number,
	peRozpis?: number,
	peSkupiny?: number,
	peUsers?: number,
	peMain?: number
};
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch: GraphQLTypes["PermissionPatch"],
	peId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyCategory` mutation. */
["UpdatePlatbyCategoryPayload"]: {
	__typename: "UpdatePlatbyCategoryPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategory` that was updated by this mutation. */
	platbyCategory?: GraphQLTypes["PlatbyCategory"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?: GraphQLTypes["PlatbyCategoriesEdge"]
};
	/** All input for the `updatePlatbyCategoryByNodeId` mutation. */
["UpdatePlatbyCategoryByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyCategory` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch: GraphQLTypes["PlatbyCategoryPatch"]
};
	/** Represents an update to a `PlatbyCategory`. Fields that are set will be updated. */
["PlatbyCategoryPatch"]: {
		pcId?: GraphQLTypes["BigInt"],
	pcName?: string,
	pcSymbol?: GraphQLTypes["BigInt"],
	pcAmount?: GraphQLTypes["BigFloat"],
	pcDateDue?: GraphQLTypes["Date"],
	pcValidFrom?: GraphQLTypes["Date"],
	pcValidTo?: GraphQLTypes["Date"],
	pcUseBase?: boolean,
	pcUsePrefix?: boolean,
	pcArchive?: boolean,
	pcVisible?: boolean
};
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch: GraphQLTypes["PlatbyCategoryPatch"],
	pcId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupPayload"]: {
	__typename: "UpdatePlatbyCategoryGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategoryGroup` that was updated by this mutation. */
	platbyCategoryGroup?: GraphQLTypes["PlatbyCategoryGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?: GraphQLTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the `updatePlatbyCategoryGroupByNodeId` mutation. */
["UpdatePlatbyCategoryGroupByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch: GraphQLTypes["PlatbyCategoryGroupPatch"]
};
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: {
		pcgId?: GraphQLTypes["BigInt"],
	pcgIdGroup?: GraphQLTypes["BigInt"],
	pcgIdCategory?: GraphQLTypes["BigInt"]
};
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch: GraphQLTypes["PlatbyCategoryGroupPatch"],
	pcgId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyGroup` mutation. */
["UpdatePlatbyGroupPayload"]: {
	__typename: "UpdatePlatbyGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroup` that was updated by this mutation. */
	platbyGroup?: GraphQLTypes["PlatbyGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?: GraphQLTypes["PlatbyGroupsEdge"]
};
	/** All input for the `updatePlatbyGroupByNodeId` mutation. */
["UpdatePlatbyGroupByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyGroup` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch: GraphQLTypes["PlatbyGroupPatch"]
};
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: {
		pgId?: GraphQLTypes["BigInt"],
	pgType?: GraphQLTypes["BigFloat"],
	pgName?: string,
	pgDescription?: string,
	pgBase?: GraphQLTypes["BigInt"]
};
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch: GraphQLTypes["PlatbyGroupPatch"],
	pgId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaPayload"]: {
	__typename: "UpdatePlatbyGroupSkupinaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroupSkupina` that was updated by this mutation. */
	platbyGroupSkupina?: GraphQLTypes["PlatbyGroupSkupina"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?: GraphQLTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?: GraphQLTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the `updatePlatbyGroupSkupinaByNodeId` mutation. */
["UpdatePlatbyGroupSkupinaByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch: GraphQLTypes["PlatbyGroupSkupinaPatch"]
};
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: {
		pgsId?: GraphQLTypes["BigInt"],
	pgsIdSkupina?: GraphQLTypes["BigInt"],
	pgsIdGroup?: GraphQLTypes["BigInt"]
};
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch: GraphQLTypes["PlatbyGroupSkupinaPatch"],
	pgsId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyItem` mutation. */
["UpdatePlatbyItemPayload"]: {
	__typename: "UpdatePlatbyItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyItem` that was updated by this mutation. */
	platbyItem?: GraphQLTypes["PlatbyItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?: GraphQLTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?: GraphQLTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?: GraphQLTypes["PlatbyItemsEdge"]
};
	/** All input for the `updatePlatbyItemByNodeId` mutation. */
["UpdatePlatbyItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyItem` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch: GraphQLTypes["PlatbyItemPatch"]
};
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: {
		piId?: GraphQLTypes["BigInt"],
	piIdUser?: GraphQLTypes["BigInt"],
	piIdCategory?: GraphQLTypes["BigInt"],
	piIdRaw?: GraphQLTypes["BigInt"],
	piAmount?: GraphQLTypes["BigFloat"],
	piDate?: GraphQLTypes["Date"],
	piPrefix?: number
};
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch: GraphQLTypes["PlatbyItemPatch"],
	piId: GraphQLTypes["BigInt"]
};
	/** The output of our update `PlatbyRaw` mutation. */
["UpdatePlatbyRawPayload"]: {
	__typename: "UpdatePlatbyRawPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyRaw` that was updated by this mutation. */
	platbyRaw?: GraphQLTypes["PlatbyRaw"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?: GraphQLTypes["PlatbyRawsEdge"]
};
	/** All input for the `updatePlatbyRawByNodeId` mutation. */
["UpdatePlatbyRawByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyRaw` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch: GraphQLTypes["PlatbyRawPatch"]
};
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: {
		prId?: GraphQLTypes["BigInt"],
	prRaw?: string,
	prHash?: string,
	prSorted?: boolean,
	prDiscarded?: boolean
};
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch: GraphQLTypes["PlatbyRawPatch"],
	prId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Rozpi` mutation. */
["UpdateRozpiPayload"]: {
	__typename: "UpdateRozpiPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Rozpi` that was updated by this mutation. */
	rozpi?: GraphQLTypes["Rozpi"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?: GraphQLTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?: GraphQLTypes["RozpisEdge"]
};
	/** All input for the `updateRozpiByNodeId` mutation. */
["UpdateRozpiByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Rozpi` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch: GraphQLTypes["RozpiPatch"]
};
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: {
		rId?: GraphQLTypes["BigInt"],
	rTrener?: GraphQLTypes["BigInt"],
	rKde?: string,
	rDatum?: GraphQLTypes["Date"],
	rVisible?: boolean,
	rLock?: boolean,
	rTimestamp?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch: GraphQLTypes["RozpiPatch"],
	rId: GraphQLTypes["BigInt"]
};
	/** The output of our update `RozpisItem` mutation. */
["UpdateRozpisItemPayload"]: {
	__typename: "UpdateRozpisItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RozpisItem` that was updated by this mutation. */
	rozpisItem?: GraphQLTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?: GraphQLTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?: GraphQLTypes["RozpisItemsEdge"]
};
	/** All input for the `updateRozpisItemByNodeId` mutation. */
["UpdateRozpisItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `RozpisItem` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch: GraphQLTypes["RozpisItemPatch"]
};
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: {
		riId?: GraphQLTypes["BigInt"],
	riIdRodic?: GraphQLTypes["BigInt"],
	riPartner?: GraphQLTypes["BigInt"],
	riOd?: GraphQLTypes["Time"],
	riDo?: GraphQLTypes["Time"],
	riLock?: boolean
};
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch: GraphQLTypes["RozpisItemPatch"],
	riId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Session` mutation. */
["UpdateSessionPayload"]: {
	__typename: "UpdateSessionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Session` that was updated by this mutation. */
	session?: GraphQLTypes["Session"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?: GraphQLTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?: GraphQLTypes["SessionsEdge"]
};
	/** All input for the `updateSessionByNodeId` mutation. */
["UpdateSessionByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Session` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Session` being updated. */
	patch: GraphQLTypes["SessionPatch"]
};
	/** Represents an update to a `Session`. Fields that are set will be updated. */
["SessionPatch"]: {
		ssId?: string,
	ssData?: string,
	ssUpdatedAt?: GraphQLTypes["Datetime"],
	ssLifetime?: GraphQLTypes["BigInt"],
	ssUser?: GraphQLTypes["BigInt"]
};
	/** All input for the `updateSession` mutation. */
["UpdateSessionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Session` being updated. */
	patch: GraphQLTypes["SessionPatch"],
	ssId: string
};
	/** The output of our update `Skupiny` mutation. */
["UpdateSkupinyPayload"]: {
	__typename: "UpdateSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Skupiny` that was updated by this mutation. */
	skupiny?: GraphQLTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?: GraphQLTypes["SkupiniesEdge"]
};
	/** All input for the `updateSkupinyByNodeId` mutation. */
["UpdateSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Skupiny` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Skupiny` being updated. */
	patch: GraphQLTypes["SkupinyPatch"]
};
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: {
		sId?: GraphQLTypes["BigInt"],
	sName?: string,
	sDescription?: string,
	sColorRgb?: string,
	sColorText?: string,
	sLocation?: string,
	sVisible?: boolean
};
	/** All input for the `updateSkupiny` mutation. */
["UpdateSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Skupiny` being updated. */
	patch: GraphQLTypes["SkupinyPatch"],
	sId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Upozorneni` mutation. */
["UpdateUpozorneniPayload"]: {
	__typename: "UpdateUpozorneniPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Upozorneni` that was updated by this mutation. */
	upozorneni?: GraphQLTypes["Upozorneni"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?: GraphQLTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?: GraphQLTypes["UpozornenisEdge"]
};
	/** All input for the `updateUpozorneniByNodeId` mutation. */
["UpdateUpozorneniByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Upozorneni` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch: GraphQLTypes["UpozorneniPatch"]
};
	/** Represents an update to a `Upozorneni`. Fields that are set will be updated. */
["UpozorneniPatch"]: {
		upId?: GraphQLTypes["BigInt"],
	upKdo?: GraphQLTypes["BigInt"],
	upNadpis?: string,
	upText?: string,
	upBarvy?: GraphQLTypes["BigInt"],
	upLock?: boolean,
	upTimestamp?: GraphQLTypes["Datetime"],
	upTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch: GraphQLTypes["UpozorneniPatch"],
	upId: GraphQLTypes["BigInt"]
};
	/** The output of our update `UpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyPayload"]: {
	__typename: "UpdateUpozorneniSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UpozorneniSkupiny` that was updated by this mutation. */
	upozorneniSkupiny?: GraphQLTypes["UpozorneniSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?: GraphQLTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?: GraphQLTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the `updateUpozorneniSkupinyByNodeId` mutation. */
["UpdateUpozorneniSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch: GraphQLTypes["UpozorneniSkupinyPatch"]
};
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: {
		upsId?: GraphQLTypes["BigInt"],
	upsIdRodic?: GraphQLTypes["BigInt"],
	upsIdSkupina?: GraphQLTypes["BigInt"],
	upsColor?: string,
	upsPopis?: string
};
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch: GraphQLTypes["UpozorneniSkupinyPatch"],
	upsId: GraphQLTypes["BigInt"]
};
	/** The output of our update `User` mutation. */
["UpdateUserPayload"]: {
	__typename: "UpdateUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `User` that was updated by this mutation. */
	user?: GraphQLTypes["User"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?: GraphQLTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?: GraphQLTypes["UsersEdge"]
};
	/** All input for the `updateUserByNodeId` mutation. */
["UpdateUserByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `User` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch: GraphQLTypes["UserPatch"]
};
	/** Represents an update to a `User`. Fields that are set will be updated. */
["UserPatch"]: {
		uId?: GraphQLTypes["BigInt"],
	uLogin?: string,
	uPass?: string,
	uJmeno?: string,
	uPrijmeni?: string,
	uPohlavi?: string,
	uEmail?: string,
	uTelefon?: string,
	uNarozeni?: GraphQLTypes["Date"],
	uRodneCislo?: string,
	uPoznamky?: string,
	uTimestamp?: GraphQLTypes["Datetime"],
	uLevel?: number,
	uGroup?: GraphQLTypes["BigInt"],
	uSkupina?: GraphQLTypes["BigInt"],
	uDancer?: boolean,
	uBan?: boolean,
	uLock?: boolean,
	uConfirmed?: boolean,
	uSystem?: boolean,
	uStreet?: string,
	uConscriptionNumber?: string,
	uOrientationNumber?: string,
	uDistrict?: string,
	uCity?: string,
	uPostalCode?: string,
	uNationality?: string,
	uMemberSince?: GraphQLTypes["Datetime"],
	uMemberUntil?: GraphQLTypes["Datetime"],
	uCreatedAt?: GraphQLTypes["Datetime"],
	uTeacher?: boolean,
	uGdprSignedAt?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch: GraphQLTypes["UserPatch"],
	uId: GraphQLTypes["BigInt"]
};
	/** The output of our update `UsersSkupiny` mutation. */
["UpdateUsersSkupinyPayload"]: {
	__typename: "UpdateUsersSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UsersSkupiny` that was updated by this mutation. */
	usersSkupiny?: GraphQLTypes["UsersSkupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?: GraphQLTypes["UsersSkupiniesEdge"]
};
	/** All input for the `updateUsersSkupinyByNodeId` mutation. */
["UpdateUsersSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `UsersSkupiny` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
	patch: GraphQLTypes["UsersSkupinyPatch"]
};
	/** Represents an update to a `UsersSkupiny`. Fields that are set will be updated. */
["UsersSkupinyPatch"]: {
		usId?: GraphQLTypes["BigInt"],
	usColor?: string,
	usPlatbaMesic?: GraphQLTypes["BigInt"],
	usPlatbaCtvrtrok?: GraphQLTypes["BigInt"],
	usPlatbaPulrok?: GraphQLTypes["BigInt"],
	usPopis?: string
};
	/** All input for the `updateUsersSkupiny` mutation. */
["UpdateUsersSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
	patch: GraphQLTypes["UsersSkupinyPatch"],
	usId: GraphQLTypes["BigInt"]
};
	/** The output of our update `Video` mutation. */
["UpdateVideoPayload"]: {
	__typename: "UpdateVideoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Video` that was updated by this mutation. */
	video?: GraphQLTypes["Video"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?: GraphQLTypes["VideosEdge"]
};
	/** All input for the `updateVideoByNodeId` mutation. */
["UpdateVideoByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Video` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `Video` being updated. */
	patch: GraphQLTypes["VideoPatch"]
};
	/** Represents an update to a `Video`. Fields that are set will be updated. */
["VideoPatch"]: {
		vId?: GraphQLTypes["BigInt"],
	vUri?: string,
	vTitle?: string,
	vAuthor?: string,
	vDescription?: string,
	vPlaylist?: string,
	vCreatedAt?: GraphQLTypes["Datetime"],
	vUpdatedAt?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateVideo` mutation. */
["UpdateVideoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Video` being updated. */
	patch: GraphQLTypes["VideoPatch"],
	vId: GraphQLTypes["BigInt"]
};
	/** The output of our update `VideoList` mutation. */
["UpdateVideoListPayload"]: {
	__typename: "UpdateVideoListPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoList` that was updated by this mutation. */
	videoList?: GraphQLTypes["VideoList"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?: GraphQLTypes["VideoListsEdge"]
};
	/** All input for the `updateVideoListByNodeId` mutation. */
["UpdateVideoListByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `VideoList` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `VideoList` being updated. */
	patch: GraphQLTypes["VideoListPatch"]
};
	/** Represents an update to a `VideoList`. Fields that are set will be updated. */
["VideoListPatch"]: {
		vlId?: GraphQLTypes["BigInt"],
	vlUrl?: string,
	vlTitle?: string,
	vlDescription?: string,
	vlCount?: GraphQLTypes["BigInt"],
	vlCreatedAt?: GraphQLTypes["Datetime"],
	vlLastChecked?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateVideoList` mutation. */
["UpdateVideoListInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `VideoList` being updated. */
	patch: GraphQLTypes["VideoListPatch"],
	vlId: GraphQLTypes["BigInt"]
};
	/** The output of our update `VideoSource` mutation. */
["UpdateVideoSourcePayload"]: {
	__typename: "UpdateVideoSourcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoSource` that was updated by this mutation. */
	videoSource?: GraphQLTypes["VideoSource"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?: GraphQLTypes["VideoSourcesEdge"]
};
	/** All input for the `updateVideoSourceByNodeId` mutation. */
["UpdateVideoSourceByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `VideoSource` to be updated. */
	nodeId: string,
	/** An object where the defined keys will be set on the `VideoSource` being updated. */
	patch: GraphQLTypes["VideoSourcePatch"]
};
	/** Represents an update to a `VideoSource`. Fields that are set will be updated. */
["VideoSourcePatch"]: {
		vsId?: GraphQLTypes["BigInt"],
	vsUrl?: string,
	vsTitle?: string,
	vsDescription?: string,
	vsCreatedAt?: GraphQLTypes["Datetime"],
	vsLastChecked?: GraphQLTypes["Datetime"]
};
	/** All input for the `updateVideoSource` mutation. */
["UpdateVideoSourceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `VideoSource` being updated. */
	patch: GraphQLTypes["VideoSourcePatch"],
	vsId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Akce` mutation. */
["DeleteAkcePayload"]: {
	__typename: "DeleteAkcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Akce` that was deleted by this mutation. */
	akce?: GraphQLTypes["Akce"],
	deletedAkceNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Akce`. May be used by Relay 1. */
	akceEdge?: GraphQLTypes["AkcesEdge"]
};
	/** All input for the `deleteAkceByNodeId` mutation. */
["DeleteAkceByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Akce` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteAkce` mutation. */
["DeleteAkceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	aId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `AkceItem` mutation. */
["DeleteAkceItemPayload"]: {
	__typename: "DeleteAkceItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AkceItem` that was deleted by this mutation. */
	akceItem?: GraphQLTypes["AkceItem"],
	deletedAkceItemNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?: GraphQLTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?: GraphQLTypes["User"],
	/** An edge for our `AkceItem`. May be used by Relay 1. */
	akceItemEdge?: GraphQLTypes["AkceItemsEdge"]
};
	/** All input for the `deleteAkceItemByNodeId` mutation. */
["DeleteAkceItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `AkceItem` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteAkceItem` mutation. */
["DeleteAkceItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	aiId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Aktuality` mutation. */
["DeleteAktualityPayload"]: {
	__typename: "DeleteAktualityPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Aktuality` that was deleted by this mutation. */
	aktuality?: GraphQLTypes["Aktuality"],
	deletedAktualityNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?: GraphQLTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?: GraphQLTypes["GalerieFoto"],
	/** An edge for our `Aktuality`. May be used by Relay 1. */
	aktualityEdge?: GraphQLTypes["AktualitiesEdge"]
};
	/** All input for the `deleteAktualityByNodeId` mutation. */
["DeleteAktualityByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Aktuality` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	atId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Dokumenty` mutation. */
["DeleteDokumentyPayload"]: {
	__typename: "DeleteDokumentyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Dokumenty` that was deleted by this mutation. */
	dokumenty?: GraphQLTypes["Dokumenty"],
	deletedDokumentyNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Dokumenty`. */
	userByDKdo?: GraphQLTypes["User"],
	/** An edge for our `Dokumenty`. May be used by Relay 1. */
	dokumentyEdge?: GraphQLTypes["DokumentiesEdge"]
};
	/** All input for the `deleteDokumentyByNodeId` mutation. */
["DeleteDokumentyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Dokumenty` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	dId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `GalerieDir` mutation. */
["DeleteGalerieDirPayload"]: {
	__typename: "DeleteGalerieDirPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieDir` that was deleted by this mutation. */
	galerieDir?: GraphQLTypes["GalerieDir"],
	deletedGalerieDirNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `GalerieDir`. May be used by Relay 1. */
	galerieDirEdge?: GraphQLTypes["GalerieDirsEdge"]
};
	/** All input for the `deleteGalerieDirByNodeId` mutation. */
["DeleteGalerieDirByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `GalerieDir` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteGalerieDir` mutation. */
["DeleteGalerieDirInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	gdId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `GalerieFoto` mutation. */
["DeleteGalerieFotoPayload"]: {
	__typename: "DeleteGalerieFotoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `GalerieFoto` that was deleted by this mutation. */
	galerieFoto?: GraphQLTypes["GalerieFoto"],
	deletedGalerieFotoNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?: GraphQLTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?: GraphQLTypes["User"],
	/** An edge for our `GalerieFoto`. May be used by Relay 1. */
	galerieFotoEdge?: GraphQLTypes["GalerieFotosEdge"]
};
	/** All input for the `deleteGalerieFotoByNodeId` mutation. */
["DeleteGalerieFotoByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `GalerieFoto` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	gfId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Nabidka` mutation. */
["DeleteNabidkaPayload"]: {
	__typename: "DeleteNabidkaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Nabidka` that was deleted by this mutation. */
	nabidka?: GraphQLTypes["Nabidka"],
	deletedNabidkaNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?: GraphQLTypes["NabidkasEdge"]
};
	/** All input for the `deleteNabidkaByNodeId` mutation. */
["DeleteNabidkaByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Nabidka` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteNabidka` mutation. */
["DeleteNabidkaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	nId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `NabidkaItem` mutation. */
["DeleteNabidkaItemPayload"]: {
	__typename: "DeleteNabidkaItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `NabidkaItem` that was deleted by this mutation. */
	nabidkaItem?: GraphQLTypes["NabidkaItem"],
	deletedNabidkaItemNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?: GraphQLTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `NabidkaItem`. May be used by Relay 1. */
	nabidkaItemEdge?: GraphQLTypes["NabidkaItemsEdge"]
};
	/** All input for the `deleteNabidkaItemByNodeId` mutation. */
["DeleteNabidkaItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `NabidkaItem` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	niId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Parameter` mutation. */
["DeleteParameterPayload"]: {
	__typename: "DeleteParameterPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Parameter` that was deleted by this mutation. */
	parameter?: GraphQLTypes["Parameter"],
	deletedParameterNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Parameter`. May be used by Relay 1. */
	parameterEdge?: GraphQLTypes["ParametersEdge"]
};
	/** All input for the `deleteParameterByNodeId` mutation. */
["DeleteParameterByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Parameter` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteParameter` mutation. */
["DeleteParameterInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	paName: string
};
	/** The output of our delete `Pary` mutation. */
["DeleteParyPayload"]: {
	__typename: "DeleteParyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Pary` that was deleted by this mutation. */
	pary?: GraphQLTypes["Pary"],
	deletedParyNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?: GraphQLTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?: GraphQLTypes["PariesEdge"]
};
	/** All input for the `deleteParyByNodeId` mutation. */
["DeleteParyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Pary` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePary` mutation. */
["DeleteParyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `ParyNavrh` mutation. */
["DeleteParyNavrhPayload"]: {
	__typename: "DeleteParyNavrhPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `ParyNavrh` that was deleted by this mutation. */
	paryNavrh?: GraphQLTypes["ParyNavrh"],
	deletedParyNavrhNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnNavrhl?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartner?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `ParyNavrh`. */
	userByPnPartnerka?: GraphQLTypes["User"],
	/** An edge for our `ParyNavrh`. May be used by Relay 1. */
	paryNavrhEdge?: GraphQLTypes["ParyNavrhsEdge"]
};
	/** All input for the `deleteParyNavrhByNodeId` mutation. */
["DeleteParyNavrhByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `ParyNavrh` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteParyNavrh` mutation. */
["DeleteParyNavrhInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pnId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Permission` mutation. */
["DeletePermissionPayload"]: {
	__typename: "DeletePermissionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Permission` that was deleted by this mutation. */
	permission?: GraphQLTypes["Permission"],
	deletedPermissionNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Permission`. May be used by Relay 1. */
	permissionEdge?: GraphQLTypes["PermissionsEdge"]
};
	/** All input for the `deletePermissionByNodeId` mutation. */
["DeletePermissionByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Permission` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	peId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyCategory` mutation. */
["DeletePlatbyCategoryPayload"]: {
	__typename: "DeletePlatbyCategoryPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategory` that was deleted by this mutation. */
	platbyCategory?: GraphQLTypes["PlatbyCategory"],
	deletedPlatbyCategoryNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyCategory`. May be used by Relay 1. */
	platbyCategoryEdge?: GraphQLTypes["PlatbyCategoriesEdge"]
};
	/** All input for the `deletePlatbyCategoryByNodeId` mutation. */
["DeletePlatbyCategoryByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyCategory` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyCategory` mutation. */
["DeletePlatbyCategoryInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pcId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupPayload"]: {
	__typename: "DeletePlatbyCategoryGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyCategoryGroup` that was deleted by this mutation. */
	platbyCategoryGroup?: GraphQLTypes["PlatbyCategoryGroup"],
	deletedPlatbyCategoryGroupNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
	platbyCategoryGroupEdge?: GraphQLTypes["PlatbyCategoryGroupsEdge"]
};
	/** All input for the `deletePlatbyCategoryGroupByNodeId` mutation. */
["DeletePlatbyCategoryGroupByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyCategoryGroup` mutation. */
["DeletePlatbyCategoryGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pcgId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyGroup` mutation. */
["DeletePlatbyGroupPayload"]: {
	__typename: "DeletePlatbyGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroup` that was deleted by this mutation. */
	platbyGroup?: GraphQLTypes["PlatbyGroup"],
	deletedPlatbyGroupNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyGroup`. May be used by Relay 1. */
	platbyGroupEdge?: GraphQLTypes["PlatbyGroupsEdge"]
};
	/** All input for the `deletePlatbyGroupByNodeId` mutation. */
["DeletePlatbyGroupByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyGroup` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyGroup` mutation. */
["DeletePlatbyGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pgId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaPayload"]: {
	__typename: "DeletePlatbyGroupSkupinaPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyGroupSkupina` that was deleted by this mutation. */
	platbyGroupSkupina?: GraphQLTypes["PlatbyGroupSkupina"],
	deletedPlatbyGroupSkupinaNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?: GraphQLTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
	platbyGroupSkupinaEdge?: GraphQLTypes["PlatbyGroupSkupinasEdge"]
};
	/** All input for the `deletePlatbyGroupSkupinaByNodeId` mutation. */
["DeletePlatbyGroupSkupinaByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyGroupSkupina` mutation. */
["DeletePlatbyGroupSkupinaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	pgsId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyItem` mutation. */
["DeletePlatbyItemPayload"]: {
	__typename: "DeletePlatbyItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyItem` that was deleted by this mutation. */
	platbyItem?: GraphQLTypes["PlatbyItem"],
	deletedPlatbyItemNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?: GraphQLTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?: GraphQLTypes["PlatbyRaw"],
	/** An edge for our `PlatbyItem`. May be used by Relay 1. */
	platbyItemEdge?: GraphQLTypes["PlatbyItemsEdge"]
};
	/** All input for the `deletePlatbyItemByNodeId` mutation. */
["DeletePlatbyItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyItem` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyItem` mutation. */
["DeletePlatbyItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	piId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `PlatbyRaw` mutation. */
["DeletePlatbyRawPayload"]: {
	__typename: "DeletePlatbyRawPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `PlatbyRaw` that was deleted by this mutation. */
	platbyRaw?: GraphQLTypes["PlatbyRaw"],
	deletedPlatbyRawNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `PlatbyRaw`. May be used by Relay 1. */
	platbyRawEdge?: GraphQLTypes["PlatbyRawsEdge"]
};
	/** All input for the `deletePlatbyRawByNodeId` mutation. */
["DeletePlatbyRawByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `PlatbyRaw` to be deleted. */
	nodeId: string
};
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	prId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Rozpi` mutation. */
["DeleteRozpiPayload"]: {
	__typename: "DeleteRozpiPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Rozpi` that was deleted by this mutation. */
	rozpi?: GraphQLTypes["Rozpi"],
	deletedRozpiNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Rozpi`. */
	userByRTrener?: GraphQLTypes["User"],
	/** An edge for our `Rozpi`. May be used by Relay 1. */
	rozpiEdge?: GraphQLTypes["RozpisEdge"]
};
	/** All input for the `deleteRozpiByNodeId` mutation. */
["DeleteRozpiByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Rozpi` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteRozpi` mutation. */
["DeleteRozpiInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	rId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `RozpisItem` mutation. */
["DeleteRozpisItemPayload"]: {
	__typename: "DeleteRozpisItemPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RozpisItem` that was deleted by this mutation. */
	rozpisItem?: GraphQLTypes["RozpisItem"],
	deletedRozpisItemNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?: GraphQLTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?: GraphQLTypes["Pary"],
	/** An edge for our `RozpisItem`. May be used by Relay 1. */
	rozpisItemEdge?: GraphQLTypes["RozpisItemsEdge"]
};
	/** All input for the `deleteRozpisItemByNodeId` mutation. */
["DeleteRozpisItemByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `RozpisItem` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	riId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Session` mutation. */
["DeleteSessionPayload"]: {
	__typename: "DeleteSessionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Session` that was deleted by this mutation. */
	session?: GraphQLTypes["Session"],
	deletedSessionNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Session`. */
	userBySsUser?: GraphQLTypes["User"],
	/** An edge for our `Session`. May be used by Relay 1. */
	sessionEdge?: GraphQLTypes["SessionsEdge"]
};
	/** All input for the `deleteSessionByNodeId` mutation. */
["DeleteSessionByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Session` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteSession` mutation. */
["DeleteSessionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	ssId: string
};
	/** The output of our delete `Skupiny` mutation. */
["DeleteSkupinyPayload"]: {
	__typename: "DeleteSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Skupiny` that was deleted by this mutation. */
	skupiny?: GraphQLTypes["Skupiny"],
	deletedSkupinyNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?: GraphQLTypes["SkupiniesEdge"]
};
	/** All input for the `deleteSkupinyByNodeId` mutation. */
["DeleteSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Skupiny` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	sId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Upozorneni` mutation. */
["DeleteUpozorneniPayload"]: {
	__typename: "DeleteUpozorneniPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Upozorneni` that was deleted by this mutation. */
	upozorneni?: GraphQLTypes["Upozorneni"],
	deletedUpozorneniNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Upozorneni`. */
	userByUpKdo?: GraphQLTypes["User"],
	/** An edge for our `Upozorneni`. May be used by Relay 1. */
	upozorneniEdge?: GraphQLTypes["UpozornenisEdge"]
};
	/** All input for the `deleteUpozorneniByNodeId` mutation. */
["DeleteUpozorneniByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Upozorneni` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteUpozorneni` mutation. */
["DeleteUpozorneniInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	upId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `UpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyPayload"]: {
	__typename: "DeleteUpozorneniSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UpozorneniSkupiny` that was deleted by this mutation. */
	upozorneniSkupiny?: GraphQLTypes["UpozorneniSkupiny"],
	deletedUpozorneniSkupinyNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?: GraphQLTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
	upozorneniSkupinyEdge?: GraphQLTypes["UpozorneniSkupiniesEdge"]
};
	/** All input for the `deleteUpozorneniSkupinyByNodeId` mutation. */
["DeleteUpozorneniSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteUpozorneniSkupiny` mutation. */
["DeleteUpozorneniSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	upsId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `User` mutation. */
["DeleteUserPayload"]: {
	__typename: "DeleteUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `User` that was deleted by this mutation. */
	user?: GraphQLTypes["User"],
	deletedUserNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?: GraphQLTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?: GraphQLTypes["Skupiny"],
	/** An edge for our `User`. May be used by Relay 1. */
	userEdge?: GraphQLTypes["UsersEdge"]
};
	/** All input for the `deleteUserByNodeId` mutation. */
["DeleteUserByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `User` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	uId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `UsersSkupiny` mutation. */
["DeleteUsersSkupinyPayload"]: {
	__typename: "DeleteUsersSkupinyPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `UsersSkupiny` that was deleted by this mutation. */
	usersSkupiny?: GraphQLTypes["UsersSkupiny"],
	deletedUsersSkupinyNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `UsersSkupiny`. May be used by Relay 1. */
	usersSkupinyEdge?: GraphQLTypes["UsersSkupiniesEdge"]
};
	/** All input for the `deleteUsersSkupinyByNodeId` mutation. */
["DeleteUsersSkupinyByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `UsersSkupiny` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteUsersSkupiny` mutation. */
["DeleteUsersSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	usId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Video` mutation. */
["DeleteVideoPayload"]: {
	__typename: "DeleteVideoPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Video` that was deleted by this mutation. */
	video?: GraphQLTypes["Video"],
	deletedVideoNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Video`. May be used by Relay 1. */
	videoEdge?: GraphQLTypes["VideosEdge"]
};
	/** All input for the `deleteVideoByNodeId` mutation. */
["DeleteVideoByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `Video` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteVideo` mutation. */
["DeleteVideoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	vId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `VideoList` mutation. */
["DeleteVideoListPayload"]: {
	__typename: "DeleteVideoListPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoList` that was deleted by this mutation. */
	videoList?: GraphQLTypes["VideoList"],
	deletedVideoListNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoList`. May be used by Relay 1. */
	videoListEdge?: GraphQLTypes["VideoListsEdge"]
};
	/** All input for the `deleteVideoListByNodeId` mutation. */
["DeleteVideoListByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `VideoList` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteVideoList` mutation. */
["DeleteVideoListInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	vlId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `VideoSource` mutation. */
["DeleteVideoSourcePayload"]: {
	__typename: "DeleteVideoSourcePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `VideoSource` that was deleted by this mutation. */
	videoSource?: GraphQLTypes["VideoSource"],
	deletedVideoSourceNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `VideoSource`. May be used by Relay 1. */
	videoSourceEdge?: GraphQLTypes["VideoSourcesEdge"]
};
	/** All input for the `deleteVideoSourceByNodeId` mutation. */
["DeleteVideoSourceByNodeIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The globally unique `ID` which will identify a single `VideoSource` to be deleted. */
	nodeId: string
};
	/** All input for the `deleteVideoSource` mutation. */
["DeleteVideoSourceInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	vsId: GraphQLTypes["BigInt"]
};
	/** The output of our `login` mutation. */
["LoginPayload"]: {
	__typename: "LoginPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	result?: GraphQLTypes["LoginRecord"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** The return type of our `login` mutation. */
["LoginRecord"]: {
	__typename: "LoginRecord",
	couple?: GraphQLTypes["Pary"],
	sess?: GraphQLTypes["Session"],
	usr?: GraphQLTypes["User"]
};
	/** All input for the `login` mutation. */
["LoginInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	login: string,
	passwd: string
};
	/** The output of our `logout` mutation. */
["LogoutPayload"]: {
	__typename: "LogoutPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `logout` mutation. */
["LogoutInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string
};
	["Upload"]: {
	__typename: "Upload",
	uploadUrl: string
};
	["UploadInput"]: {
		directory?: string,
	filename: string
}
    }
/** Methods to use when ordering `User`. */
export const enum UsersOrderBy {
	NATURAL = "NATURAL",
	U_ID_ASC = "U_ID_ASC",
	U_ID_DESC = "U_ID_DESC",
	U_LOGIN_ASC = "U_LOGIN_ASC",
	U_LOGIN_DESC = "U_LOGIN_DESC",
	U_PASS_ASC = "U_PASS_ASC",
	U_PASS_DESC = "U_PASS_DESC",
	U_JMENO_ASC = "U_JMENO_ASC",
	U_JMENO_DESC = "U_JMENO_DESC",
	U_PRIJMENI_ASC = "U_PRIJMENI_ASC",
	U_PRIJMENI_DESC = "U_PRIJMENI_DESC",
	U_POHLAVI_ASC = "U_POHLAVI_ASC",
	U_POHLAVI_DESC = "U_POHLAVI_DESC",
	U_EMAIL_ASC = "U_EMAIL_ASC",
	U_EMAIL_DESC = "U_EMAIL_DESC",
	U_TELEFON_ASC = "U_TELEFON_ASC",
	U_TELEFON_DESC = "U_TELEFON_DESC",
	U_NAROZENI_ASC = "U_NAROZENI_ASC",
	U_NAROZENI_DESC = "U_NAROZENI_DESC",
	U_RODNE_CISLO_ASC = "U_RODNE_CISLO_ASC",
	U_RODNE_CISLO_DESC = "U_RODNE_CISLO_DESC",
	U_POZNAMKY_ASC = "U_POZNAMKY_ASC",
	U_POZNAMKY_DESC = "U_POZNAMKY_DESC",
	U_TIMESTAMP_ASC = "U_TIMESTAMP_ASC",
	U_TIMESTAMP_DESC = "U_TIMESTAMP_DESC",
	U_LEVEL_ASC = "U_LEVEL_ASC",
	U_LEVEL_DESC = "U_LEVEL_DESC",
	U_GROUP_ASC = "U_GROUP_ASC",
	U_GROUP_DESC = "U_GROUP_DESC",
	U_SKUPINA_ASC = "U_SKUPINA_ASC",
	U_SKUPINA_DESC = "U_SKUPINA_DESC",
	U_DANCER_ASC = "U_DANCER_ASC",
	U_DANCER_DESC = "U_DANCER_DESC",
	U_BAN_ASC = "U_BAN_ASC",
	U_BAN_DESC = "U_BAN_DESC",
	U_LOCK_ASC = "U_LOCK_ASC",
	U_LOCK_DESC = "U_LOCK_DESC",
	U_CONFIRMED_ASC = "U_CONFIRMED_ASC",
	U_CONFIRMED_DESC = "U_CONFIRMED_DESC",
	U_SYSTEM_ASC = "U_SYSTEM_ASC",
	U_SYSTEM_DESC = "U_SYSTEM_DESC",
	U_STREET_ASC = "U_STREET_ASC",
	U_STREET_DESC = "U_STREET_DESC",
	U_CONSCRIPTION_NUMBER_ASC = "U_CONSCRIPTION_NUMBER_ASC",
	U_CONSCRIPTION_NUMBER_DESC = "U_CONSCRIPTION_NUMBER_DESC",
	U_ORIENTATION_NUMBER_ASC = "U_ORIENTATION_NUMBER_ASC",
	U_ORIENTATION_NUMBER_DESC = "U_ORIENTATION_NUMBER_DESC",
	U_DISTRICT_ASC = "U_DISTRICT_ASC",
	U_DISTRICT_DESC = "U_DISTRICT_DESC",
	U_CITY_ASC = "U_CITY_ASC",
	U_CITY_DESC = "U_CITY_DESC",
	U_POSTAL_CODE_ASC = "U_POSTAL_CODE_ASC",
	U_POSTAL_CODE_DESC = "U_POSTAL_CODE_DESC",
	U_NATIONALITY_ASC = "U_NATIONALITY_ASC",
	U_NATIONALITY_DESC = "U_NATIONALITY_DESC",
	U_MEMBER_SINCE_ASC = "U_MEMBER_SINCE_ASC",
	U_MEMBER_SINCE_DESC = "U_MEMBER_SINCE_DESC",
	U_MEMBER_UNTIL_ASC = "U_MEMBER_UNTIL_ASC",
	U_MEMBER_UNTIL_DESC = "U_MEMBER_UNTIL_DESC",
	U_CREATED_AT_ASC = "U_CREATED_AT_ASC",
	U_CREATED_AT_DESC = "U_CREATED_AT_DESC",
	U_TEACHER_ASC = "U_TEACHER_ASC",
	U_TEACHER_DESC = "U_TEACHER_DESC",
	U_GDPR_SIGNED_AT_ASC = "U_GDPR_SIGNED_AT_ASC",
	U_GDPR_SIGNED_AT_DESC = "U_GDPR_SIGNED_AT_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyCategoryGroup`. */
export const enum PlatbyCategoryGroupsOrderBy {
	NATURAL = "NATURAL",
	PCG_ID_ASC = "PCG_ID_ASC",
	PCG_ID_DESC = "PCG_ID_DESC",
	PCG_ID_GROUP_ASC = "PCG_ID_GROUP_ASC",
	PCG_ID_GROUP_DESC = "PCG_ID_GROUP_DESC",
	PCG_ID_CATEGORY_ASC = "PCG_ID_CATEGORY_ASC",
	PCG_ID_CATEGORY_DESC = "PCG_ID_CATEGORY_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyItem`. */
export const enum PlatbyItemsOrderBy {
	NATURAL = "NATURAL",
	PI_ID_ASC = "PI_ID_ASC",
	PI_ID_DESC = "PI_ID_DESC",
	PI_ID_USER_ASC = "PI_ID_USER_ASC",
	PI_ID_USER_DESC = "PI_ID_USER_DESC",
	PI_ID_CATEGORY_ASC = "PI_ID_CATEGORY_ASC",
	PI_ID_CATEGORY_DESC = "PI_ID_CATEGORY_DESC",
	PI_ID_RAW_ASC = "PI_ID_RAW_ASC",
	PI_ID_RAW_DESC = "PI_ID_RAW_DESC",
	PI_AMOUNT_ASC = "PI_AMOUNT_ASC",
	PI_AMOUNT_DESC = "PI_AMOUNT_DESC",
	PI_DATE_ASC = "PI_DATE_ASC",
	PI_DATE_DESC = "PI_DATE_DESC",
	PI_PREFIX_ASC = "PI_PREFIX_ASC",
	PI_PREFIX_DESC = "PI_PREFIX_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyGroupSkupina`. */
export const enum PlatbyGroupSkupinasOrderBy {
	NATURAL = "NATURAL",
	PGS_ID_ASC = "PGS_ID_ASC",
	PGS_ID_DESC = "PGS_ID_DESC",
	PGS_ID_SKUPINA_ASC = "PGS_ID_SKUPINA_ASC",
	PGS_ID_SKUPINA_DESC = "PGS_ID_SKUPINA_DESC",
	PGS_ID_GROUP_ASC = "PGS_ID_GROUP_ASC",
	PGS_ID_GROUP_DESC = "PGS_ID_GROUP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `UpozorneniSkupiny`. */
export const enum UpozorneniSkupiniesOrderBy {
	NATURAL = "NATURAL",
	UPS_ID_ASC = "UPS_ID_ASC",
	UPS_ID_DESC = "UPS_ID_DESC",
	UPS_ID_RODIC_ASC = "UPS_ID_RODIC_ASC",
	UPS_ID_RODIC_DESC = "UPS_ID_RODIC_DESC",
	UPS_ID_SKUPINA_ASC = "UPS_ID_SKUPINA_ASC",
	UPS_ID_SKUPINA_DESC = "UPS_ID_SKUPINA_DESC",
	UPS_COLOR_ASC = "UPS_COLOR_ASC",
	UPS_COLOR_DESC = "UPS_COLOR_DESC",
	UPS_POPIS_ASC = "UPS_POPIS_ASC",
	UPS_POPIS_DESC = "UPS_POPIS_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `GalerieFoto`. */
export const enum GalerieFotosOrderBy {
	NATURAL = "NATURAL",
	GF_ID_ASC = "GF_ID_ASC",
	GF_ID_DESC = "GF_ID_DESC",
	GF_ID_RODIC_ASC = "GF_ID_RODIC_ASC",
	GF_ID_RODIC_DESC = "GF_ID_RODIC_DESC",
	GF_NAME_ASC = "GF_NAME_ASC",
	GF_NAME_DESC = "GF_NAME_DESC",
	GF_PATH_ASC = "GF_PATH_ASC",
	GF_PATH_DESC = "GF_PATH_DESC",
	GF_KDO_ASC = "GF_KDO_ASC",
	GF_KDO_DESC = "GF_KDO_DESC",
	GF_TIMESTAMP_ASC = "GF_TIMESTAMP_ASC",
	GF_TIMESTAMP_DESC = "GF_TIMESTAMP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Aktuality`. */
export const enum AktualitiesOrderBy {
	NATURAL = "NATURAL",
	AT_ID_ASC = "AT_ID_ASC",
	AT_ID_DESC = "AT_ID_DESC",
	AT_KDO_ASC = "AT_KDO_ASC",
	AT_KDO_DESC = "AT_KDO_DESC",
	AT_KAT_ASC = "AT_KAT_ASC",
	AT_KAT_DESC = "AT_KAT_DESC",
	AT_JMENO_ASC = "AT_JMENO_ASC",
	AT_JMENO_DESC = "AT_JMENO_DESC",
	AT_TEXT_ASC = "AT_TEXT_ASC",
	AT_TEXT_DESC = "AT_TEXT_DESC",
	AT_PREVIEW_ASC = "AT_PREVIEW_ASC",
	AT_PREVIEW_DESC = "AT_PREVIEW_DESC",
	AT_FOTO_ASC = "AT_FOTO_ASC",
	AT_FOTO_DESC = "AT_FOTO_DESC",
	AT_FOTO_MAIN_ASC = "AT_FOTO_MAIN_ASC",
	AT_FOTO_MAIN_DESC = "AT_FOTO_MAIN_DESC",
	AT_TIMESTAMP_ASC = "AT_TIMESTAMP_ASC",
	AT_TIMESTAMP_DESC = "AT_TIMESTAMP_DESC",
	AT_TIMESTAMP_ADD_ASC = "AT_TIMESTAMP_ADD_ASC",
	AT_TIMESTAMP_ADD_DESC = "AT_TIMESTAMP_ADD_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
export const enum ParyPSttTrida {
	Z = "Z",
	H = "H",
	D = "D",
	C = "C",
	B = "B",
	A = "A",
	M = "M"
}
export const enum ParyPLatTrida {
	Z = "Z",
	H = "H",
	D = "D",
	C = "C",
	B = "B",
	A = "A",
	M = "M"
}
/** Methods to use when ordering `NabidkaItem`. */
export const enum NabidkaItemsOrderBy {
	NATURAL = "NATURAL",
	NI_ID_ASC = "NI_ID_ASC",
	NI_ID_DESC = "NI_ID_DESC",
	NI_ID_RODIC_ASC = "NI_ID_RODIC_ASC",
	NI_ID_RODIC_DESC = "NI_ID_RODIC_DESC",
	NI_PARTNER_ASC = "NI_PARTNER_ASC",
	NI_PARTNER_DESC = "NI_PARTNER_DESC",
	NI_POCET_HOD_ASC = "NI_POCET_HOD_ASC",
	NI_POCET_HOD_DESC = "NI_POCET_HOD_DESC",
	NI_LOCK_ASC = "NI_LOCK_ASC",
	NI_LOCK_DESC = "NI_LOCK_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `RozpisItem`. */
export const enum RozpisItemsOrderBy {
	NATURAL = "NATURAL",
	RI_ID_ASC = "RI_ID_ASC",
	RI_ID_DESC = "RI_ID_DESC",
	RI_ID_RODIC_ASC = "RI_ID_RODIC_ASC",
	RI_ID_RODIC_DESC = "RI_ID_RODIC_DESC",
	RI_PARTNER_ASC = "RI_PARTNER_ASC",
	RI_PARTNER_DESC = "RI_PARTNER_DESC",
	RI_OD_ASC = "RI_OD_ASC",
	RI_OD_DESC = "RI_OD_DESC",
	RI_DO_ASC = "RI_DO_ASC",
	RI_DO_DESC = "RI_DO_DESC",
	RI_LOCK_ASC = "RI_LOCK_ASC",
	RI_LOCK_DESC = "RI_LOCK_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Nabidka`. */
export const enum NabidkasOrderBy {
	NATURAL = "NATURAL",
	N_ID_ASC = "N_ID_ASC",
	N_ID_DESC = "N_ID_DESC",
	N_TRENER_ASC = "N_TRENER_ASC",
	N_TRENER_DESC = "N_TRENER_DESC",
	N_POCET_HOD_ASC = "N_POCET_HOD_ASC",
	N_POCET_HOD_DESC = "N_POCET_HOD_DESC",
	N_MAX_POCET_HOD_ASC = "N_MAX_POCET_HOD_ASC",
	N_MAX_POCET_HOD_DESC = "N_MAX_POCET_HOD_DESC",
	N_OD_ASC = "N_OD_ASC",
	N_OD_DESC = "N_OD_DESC",
	N_DO_ASC = "N_DO_ASC",
	N_DO_DESC = "N_DO_DESC",
	N_VISIBLE_ASC = "N_VISIBLE_ASC",
	N_VISIBLE_DESC = "N_VISIBLE_DESC",
	N_LOCK_ASC = "N_LOCK_ASC",
	N_LOCK_DESC = "N_LOCK_DESC",
	N_TIMESTAMP_ASC = "N_TIMESTAMP_ASC",
	N_TIMESTAMP_DESC = "N_TIMESTAMP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Rozpi`. */
export const enum RozpisOrderBy {
	NATURAL = "NATURAL",
	R_ID_ASC = "R_ID_ASC",
	R_ID_DESC = "R_ID_DESC",
	R_TRENER_ASC = "R_TRENER_ASC",
	R_TRENER_DESC = "R_TRENER_DESC",
	R_KDE_ASC = "R_KDE_ASC",
	R_KDE_DESC = "R_KDE_DESC",
	R_DATUM_ASC = "R_DATUM_ASC",
	R_DATUM_DESC = "R_DATUM_DESC",
	R_VISIBLE_ASC = "R_VISIBLE_ASC",
	R_VISIBLE_DESC = "R_VISIBLE_DESC",
	R_LOCK_ASC = "R_LOCK_ASC",
	R_LOCK_DESC = "R_LOCK_DESC",
	R_TIMESTAMP_ASC = "R_TIMESTAMP_ASC",
	R_TIMESTAMP_DESC = "R_TIMESTAMP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Session`. */
export const enum SessionsOrderBy {
	NATURAL = "NATURAL",
	SS_ID_ASC = "SS_ID_ASC",
	SS_ID_DESC = "SS_ID_DESC",
	SS_DATA_ASC = "SS_DATA_ASC",
	SS_DATA_DESC = "SS_DATA_DESC",
	SS_UPDATED_AT_ASC = "SS_UPDATED_AT_ASC",
	SS_UPDATED_AT_DESC = "SS_UPDATED_AT_DESC",
	SS_LIFETIME_ASC = "SS_LIFETIME_ASC",
	SS_LIFETIME_DESC = "SS_LIFETIME_DESC",
	SS_USER_ASC = "SS_USER_ASC",
	SS_USER_DESC = "SS_USER_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `AkceItem`. */
export const enum AkceItemsOrderBy {
	NATURAL = "NATURAL",
	AI_ID_ASC = "AI_ID_ASC",
	AI_ID_DESC = "AI_ID_DESC",
	AI_ID_RODIC_ASC = "AI_ID_RODIC_ASC",
	AI_ID_RODIC_DESC = "AI_ID_RODIC_DESC",
	AI_USER_ASC = "AI_USER_ASC",
	AI_USER_DESC = "AI_USER_DESC",
	AI_ROK_NAROZENI_ASC = "AI_ROK_NAROZENI_ASC",
	AI_ROK_NAROZENI_DESC = "AI_ROK_NAROZENI_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Dokumenty`. */
export const enum DokumentiesOrderBy {
	NATURAL = "NATURAL",
	D_ID_ASC = "D_ID_ASC",
	D_ID_DESC = "D_ID_DESC",
	D_PATH_ASC = "D_PATH_ASC",
	D_PATH_DESC = "D_PATH_DESC",
	D_NAME_ASC = "D_NAME_ASC",
	D_NAME_DESC = "D_NAME_DESC",
	D_FILENAME_ASC = "D_FILENAME_ASC",
	D_FILENAME_DESC = "D_FILENAME_DESC",
	D_KATEGORIE_ASC = "D_KATEGORIE_ASC",
	D_KATEGORIE_DESC = "D_KATEGORIE_DESC",
	D_KDO_ASC = "D_KDO_ASC",
	D_KDO_DESC = "D_KDO_DESC",
	D_TIMESTAMP_ASC = "D_TIMESTAMP_ASC",
	D_TIMESTAMP_DESC = "D_TIMESTAMP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Pary`. */
export const enum PariesOrderBy {
	NATURAL = "NATURAL",
	P_ID_ASC = "P_ID_ASC",
	P_ID_DESC = "P_ID_DESC",
	P_ID_PARTNER_ASC = "P_ID_PARTNER_ASC",
	P_ID_PARTNER_DESC = "P_ID_PARTNER_DESC",
	P_ID_PARTNERKA_ASC = "P_ID_PARTNERKA_ASC",
	P_ID_PARTNERKA_DESC = "P_ID_PARTNERKA_DESC",
	P_STT_TRIDA_ASC = "P_STT_TRIDA_ASC",
	P_STT_TRIDA_DESC = "P_STT_TRIDA_DESC",
	P_STT_BODY_ASC = "P_STT_BODY_ASC",
	P_STT_BODY_DESC = "P_STT_BODY_DESC",
	P_STT_FINALE_ASC = "P_STT_FINALE_ASC",
	P_STT_FINALE_DESC = "P_STT_FINALE_DESC",
	P_LAT_TRIDA_ASC = "P_LAT_TRIDA_ASC",
	P_LAT_TRIDA_DESC = "P_LAT_TRIDA_DESC",
	P_LAT_BODY_ASC = "P_LAT_BODY_ASC",
	P_LAT_BODY_DESC = "P_LAT_BODY_DESC",
	P_LAT_FINALE_ASC = "P_LAT_FINALE_ASC",
	P_LAT_FINALE_DESC = "P_LAT_FINALE_DESC",
	P_HODNOCENI_ASC = "P_HODNOCENI_ASC",
	P_HODNOCENI_DESC = "P_HODNOCENI_DESC",
	P_ARCHIV_ASC = "P_ARCHIV_ASC",
	P_ARCHIV_DESC = "P_ARCHIV_DESC",
	P_TIMESTAMP_ADD_ASC = "P_TIMESTAMP_ADD_ASC",
	P_TIMESTAMP_ADD_DESC = "P_TIMESTAMP_ADD_DESC",
	P_TIMESTAMP_ARCHIVE_ASC = "P_TIMESTAMP_ARCHIVE_ASC",
	P_TIMESTAMP_ARCHIVE_DESC = "P_TIMESTAMP_ARCHIVE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `ParyNavrh`. */
export const enum ParyNavrhsOrderBy {
	NATURAL = "NATURAL",
	PN_ID_ASC = "PN_ID_ASC",
	PN_ID_DESC = "PN_ID_DESC",
	PN_NAVRHL_ASC = "PN_NAVRHL_ASC",
	PN_NAVRHL_DESC = "PN_NAVRHL_DESC",
	PN_PARTNER_ASC = "PN_PARTNER_ASC",
	PN_PARTNER_DESC = "PN_PARTNER_DESC",
	PN_PARTNERKA_ASC = "PN_PARTNERKA_ASC",
	PN_PARTNERKA_DESC = "PN_PARTNERKA_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Upozorneni`. */
export const enum UpozornenisOrderBy {
	NATURAL = "NATURAL",
	UP_ID_ASC = "UP_ID_ASC",
	UP_ID_DESC = "UP_ID_DESC",
	UP_KDO_ASC = "UP_KDO_ASC",
	UP_KDO_DESC = "UP_KDO_DESC",
	UP_NADPIS_ASC = "UP_NADPIS_ASC",
	UP_NADPIS_DESC = "UP_NADPIS_DESC",
	UP_TEXT_ASC = "UP_TEXT_ASC",
	UP_TEXT_DESC = "UP_TEXT_DESC",
	UP_BARVY_ASC = "UP_BARVY_ASC",
	UP_BARVY_DESC = "UP_BARVY_DESC",
	UP_LOCK_ASC = "UP_LOCK_ASC",
	UP_LOCK_DESC = "UP_LOCK_DESC",
	UP_TIMESTAMP_ASC = "UP_TIMESTAMP_ASC",
	UP_TIMESTAMP_DESC = "UP_TIMESTAMP_DESC",
	UP_TIMESTAMP_ADD_ASC = "UP_TIMESTAMP_ADD_ASC",
	UP_TIMESTAMP_ADD_DESC = "UP_TIMESTAMP_ADD_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Akce`. */
export const enum AkcesOrderBy {
	NATURAL = "NATURAL",
	A_ID_ASC = "A_ID_ASC",
	A_ID_DESC = "A_ID_DESC",
	A_JMENO_ASC = "A_JMENO_ASC",
	A_JMENO_DESC = "A_JMENO_DESC",
	A_KDE_ASC = "A_KDE_ASC",
	A_KDE_DESC = "A_KDE_DESC",
	A_INFO_ASC = "A_INFO_ASC",
	A_INFO_DESC = "A_INFO_DESC",
	A_OD_ASC = "A_OD_ASC",
	A_OD_DESC = "A_OD_DESC",
	A_DO_ASC = "A_DO_ASC",
	A_DO_DESC = "A_DO_DESC",
	A_KAPACITA_ASC = "A_KAPACITA_ASC",
	A_KAPACITA_DESC = "A_KAPACITA_DESC",
	A_DOKUMENTY_ASC = "A_DOKUMENTY_ASC",
	A_DOKUMENTY_DESC = "A_DOKUMENTY_DESC",
	A_TIMESTAMP_ASC = "A_TIMESTAMP_ASC",
	A_TIMESTAMP_DESC = "A_TIMESTAMP_DESC",
	A_LOCK_ASC = "A_LOCK_ASC",
	A_LOCK_DESC = "A_LOCK_DESC",
	A_VISIBLE_ASC = "A_VISIBLE_ASC",
	A_VISIBLE_DESC = "A_VISIBLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `GalerieDir`. */
export const enum GalerieDirsOrderBy {
	NATURAL = "NATURAL",
	GD_ID_ASC = "GD_ID_ASC",
	GD_ID_DESC = "GD_ID_DESC",
	GD_ID_RODIC_ASC = "GD_ID_RODIC_ASC",
	GD_ID_RODIC_DESC = "GD_ID_RODIC_DESC",
	GD_NAME_ASC = "GD_NAME_ASC",
	GD_NAME_DESC = "GD_NAME_DESC",
	GD_LEVEL_ASC = "GD_LEVEL_ASC",
	GD_LEVEL_DESC = "GD_LEVEL_DESC",
	GD_PATH_ASC = "GD_PATH_ASC",
	GD_PATH_DESC = "GD_PATH_DESC",
	GD_HIDDEN_ASC = "GD_HIDDEN_ASC",
	GD_HIDDEN_DESC = "GD_HIDDEN_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Member`. */
export const enum MembersOrderBy {
	NATURAL = "NATURAL",
	U_ID_ASC = "U_ID_ASC",
	U_ID_DESC = "U_ID_DESC",
	U_LOGIN_ASC = "U_LOGIN_ASC",
	U_LOGIN_DESC = "U_LOGIN_DESC",
	U_PASS_ASC = "U_PASS_ASC",
	U_PASS_DESC = "U_PASS_DESC",
	U_JMENO_ASC = "U_JMENO_ASC",
	U_JMENO_DESC = "U_JMENO_DESC",
	U_PRIJMENI_ASC = "U_PRIJMENI_ASC",
	U_PRIJMENI_DESC = "U_PRIJMENI_DESC",
	U_POHLAVI_ASC = "U_POHLAVI_ASC",
	U_POHLAVI_DESC = "U_POHLAVI_DESC",
	U_EMAIL_ASC = "U_EMAIL_ASC",
	U_EMAIL_DESC = "U_EMAIL_DESC",
	U_TELEFON_ASC = "U_TELEFON_ASC",
	U_TELEFON_DESC = "U_TELEFON_DESC",
	U_NAROZENI_ASC = "U_NAROZENI_ASC",
	U_NAROZENI_DESC = "U_NAROZENI_DESC",
	U_RODNE_CISLO_ASC = "U_RODNE_CISLO_ASC",
	U_RODNE_CISLO_DESC = "U_RODNE_CISLO_DESC",
	U_POZNAMKY_ASC = "U_POZNAMKY_ASC",
	U_POZNAMKY_DESC = "U_POZNAMKY_DESC",
	U_TIMESTAMP_ASC = "U_TIMESTAMP_ASC",
	U_TIMESTAMP_DESC = "U_TIMESTAMP_DESC",
	U_LEVEL_ASC = "U_LEVEL_ASC",
	U_LEVEL_DESC = "U_LEVEL_DESC",
	U_GROUP_ASC = "U_GROUP_ASC",
	U_GROUP_DESC = "U_GROUP_DESC",
	U_SKUPINA_ASC = "U_SKUPINA_ASC",
	U_SKUPINA_DESC = "U_SKUPINA_DESC",
	U_DANCER_ASC = "U_DANCER_ASC",
	U_DANCER_DESC = "U_DANCER_DESC",
	U_BAN_ASC = "U_BAN_ASC",
	U_BAN_DESC = "U_BAN_DESC",
	U_LOCK_ASC = "U_LOCK_ASC",
	U_LOCK_DESC = "U_LOCK_DESC",
	U_CONFIRMED_ASC = "U_CONFIRMED_ASC",
	U_CONFIRMED_DESC = "U_CONFIRMED_DESC",
	U_SYSTEM_ASC = "U_SYSTEM_ASC",
	U_SYSTEM_DESC = "U_SYSTEM_DESC",
	U_STREET_ASC = "U_STREET_ASC",
	U_STREET_DESC = "U_STREET_DESC",
	U_CONSCRIPTION_NUMBER_ASC = "U_CONSCRIPTION_NUMBER_ASC",
	U_CONSCRIPTION_NUMBER_DESC = "U_CONSCRIPTION_NUMBER_DESC",
	U_ORIENTATION_NUMBER_ASC = "U_ORIENTATION_NUMBER_ASC",
	U_ORIENTATION_NUMBER_DESC = "U_ORIENTATION_NUMBER_DESC",
	U_DISTRICT_ASC = "U_DISTRICT_ASC",
	U_DISTRICT_DESC = "U_DISTRICT_DESC",
	U_CITY_ASC = "U_CITY_ASC",
	U_CITY_DESC = "U_CITY_DESC",
	U_POSTAL_CODE_ASC = "U_POSTAL_CODE_ASC",
	U_POSTAL_CODE_DESC = "U_POSTAL_CODE_DESC",
	U_NATIONALITY_ASC = "U_NATIONALITY_ASC",
	U_NATIONALITY_DESC = "U_NATIONALITY_DESC",
	U_MEMBER_SINCE_ASC = "U_MEMBER_SINCE_ASC",
	U_MEMBER_SINCE_DESC = "U_MEMBER_SINCE_DESC",
	U_MEMBER_UNTIL_ASC = "U_MEMBER_UNTIL_ASC",
	U_MEMBER_UNTIL_DESC = "U_MEMBER_UNTIL_DESC",
	U_CREATED_AT_ASC = "U_CREATED_AT_ASC",
	U_CREATED_AT_DESC = "U_CREATED_AT_DESC",
	U_TEACHER_ASC = "U_TEACHER_ASC",
	U_TEACHER_DESC = "U_TEACHER_DESC",
	U_GDPR_SIGNED_AT_ASC = "U_GDPR_SIGNED_AT_ASC",
	U_GDPR_SIGNED_AT_DESC = "U_GDPR_SIGNED_AT_DESC",
	S_ID_ASC = "S_ID_ASC",
	S_ID_DESC = "S_ID_DESC",
	S_NAME_ASC = "S_NAME_ASC",
	S_NAME_DESC = "S_NAME_DESC",
	PAYMENT_VALID_ASC = "PAYMENT_VALID_ASC",
	PAYMENT_VALID_DESC = "PAYMENT_VALID_DESC"
}
/** Methods to use when ordering `Page`. */
export const enum PagesOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	URL_ASC = "URL_ASC",
	URL_DESC = "URL_DESC",
	CONTENT_ASC = "CONTENT_ASC",
	CONTENT_DESC = "CONTENT_DESC",
	CREATED_AT_ASC = "CREATED_AT_ASC",
	CREATED_AT_DESC = "CREATED_AT_DESC",
	UPDATED_AT_ASC = "UPDATED_AT_ASC",
	UPDATED_AT_DESC = "UPDATED_AT_DESC",
	TITLE_ASC = "TITLE_ASC",
	TITLE_DESC = "TITLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PageRevision`. */
export const enum PageRevisionsOrderBy {
	NATURAL = "NATURAL",
	REV_NUMBER_ASC = "REV_NUMBER_ASC",
	REV_NUMBER_DESC = "REV_NUMBER_DESC",
	REV_OPERATION_ASC = "REV_OPERATION_ASC",
	REV_OPERATION_DESC = "REV_OPERATION_DESC",
	REV_TIMESTAMP_ASC = "REV_TIMESTAMP_ASC",
	REV_TIMESTAMP_DESC = "REV_TIMESTAMP_DESC",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	URL_ASC = "URL_ASC",
	URL_DESC = "URL_DESC",
	CONTENT_ASC = "CONTENT_ASC",
	CONTENT_DESC = "CONTENT_DESC",
	CREATED_AT_ASC = "CREATED_AT_ASC",
	CREATED_AT_DESC = "CREATED_AT_DESC",
	UPDATED_AT_ASC = "UPDATED_AT_ASC",
	UPDATED_AT_DESC = "UPDATED_AT_DESC",
	TITLE_ASC = "TITLE_ASC",
	TITLE_DESC = "TITLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Parameter`. */
export const enum ParametersOrderBy {
	NATURAL = "NATURAL",
	PA_NAME_ASC = "PA_NAME_ASC",
	PA_NAME_DESC = "PA_NAME_DESC",
	PA_VALUE_ASC = "PA_VALUE_ASC",
	PA_VALUE_DESC = "PA_VALUE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Permission`. */
export const enum PermissionsOrderBy {
	NATURAL = "NATURAL",
	PE_ID_ASC = "PE_ID_ASC",
	PE_ID_DESC = "PE_ID_DESC",
	PE_NAME_ASC = "PE_NAME_ASC",
	PE_NAME_DESC = "PE_NAME_DESC",
	PE_DESCRIPTION_ASC = "PE_DESCRIPTION_ASC",
	PE_DESCRIPTION_DESC = "PE_DESCRIPTION_DESC",
	PE_AKCE_ASC = "PE_AKCE_ASC",
	PE_AKCE_DESC = "PE_AKCE_DESC",
	PE_AKTUALITY_ASC = "PE_AKTUALITY_ASC",
	PE_AKTUALITY_DESC = "PE_AKTUALITY_DESC",
	PE_ANKETY_ASC = "PE_ANKETY_ASC",
	PE_ANKETY_DESC = "PE_ANKETY_DESC",
	PE_DOKUMENTY_ASC = "PE_DOKUMENTY_ASC",
	PE_DOKUMENTY_DESC = "PE_DOKUMENTY_DESC",
	PE_GALERIE_ASC = "PE_GALERIE_ASC",
	PE_GALERIE_DESC = "PE_GALERIE_DESC",
	PE_INZERCE_ASC = "PE_INZERCE_ASC",
	PE_INZERCE_DESC = "PE_INZERCE_DESC",
	PE_KONZOLE_ASC = "PE_KONZOLE_ASC",
	PE_KONZOLE_DESC = "PE_KONZOLE_DESC",
	PE_NABIDKA_ASC = "PE_NABIDKA_ASC",
	PE_NABIDKA_DESC = "PE_NABIDKA_DESC",
	PE_NASTENKA_ASC = "PE_NASTENKA_ASC",
	PE_NASTENKA_DESC = "PE_NASTENKA_DESC",
	PE_NOVINKY_ASC = "PE_NOVINKY_ASC",
	PE_NOVINKY_DESC = "PE_NOVINKY_DESC",
	PE_PARY_ASC = "PE_PARY_ASC",
	PE_PARY_DESC = "PE_PARY_DESC",
	PE_PLATBY_ASC = "PE_PLATBY_ASC",
	PE_PLATBY_DESC = "PE_PLATBY_DESC",
	PE_PERMISSIONS_ASC = "PE_PERMISSIONS_ASC",
	PE_PERMISSIONS_DESC = "PE_PERMISSIONS_DESC",
	PE_ROZPIS_ASC = "PE_ROZPIS_ASC",
	PE_ROZPIS_DESC = "PE_ROZPIS_DESC",
	PE_SKUPINY_ASC = "PE_SKUPINY_ASC",
	PE_SKUPINY_DESC = "PE_SKUPINY_DESC",
	PE_USERS_ASC = "PE_USERS_ASC",
	PE_USERS_DESC = "PE_USERS_DESC",
	PE_MAIN_ASC = "PE_MAIN_ASC",
	PE_MAIN_DESC = "PE_MAIN_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyCategory`. */
export const enum PlatbyCategoriesOrderBy {
	NATURAL = "NATURAL",
	PC_ID_ASC = "PC_ID_ASC",
	PC_ID_DESC = "PC_ID_DESC",
	PC_NAME_ASC = "PC_NAME_ASC",
	PC_NAME_DESC = "PC_NAME_DESC",
	PC_SYMBOL_ASC = "PC_SYMBOL_ASC",
	PC_SYMBOL_DESC = "PC_SYMBOL_DESC",
	PC_AMOUNT_ASC = "PC_AMOUNT_ASC",
	PC_AMOUNT_DESC = "PC_AMOUNT_DESC",
	PC_DATE_DUE_ASC = "PC_DATE_DUE_ASC",
	PC_DATE_DUE_DESC = "PC_DATE_DUE_DESC",
	PC_VALID_FROM_ASC = "PC_VALID_FROM_ASC",
	PC_VALID_FROM_DESC = "PC_VALID_FROM_DESC",
	PC_VALID_TO_ASC = "PC_VALID_TO_ASC",
	PC_VALID_TO_DESC = "PC_VALID_TO_DESC",
	PC_USE_BASE_ASC = "PC_USE_BASE_ASC",
	PC_USE_BASE_DESC = "PC_USE_BASE_DESC",
	PC_USE_PREFIX_ASC = "PC_USE_PREFIX_ASC",
	PC_USE_PREFIX_DESC = "PC_USE_PREFIX_DESC",
	PC_ARCHIVE_ASC = "PC_ARCHIVE_ASC",
	PC_ARCHIVE_DESC = "PC_ARCHIVE_DESC",
	PC_VISIBLE_ASC = "PC_VISIBLE_ASC",
	PC_VISIBLE_DESC = "PC_VISIBLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyGroup`. */
export const enum PlatbyGroupsOrderBy {
	NATURAL = "NATURAL",
	PG_ID_ASC = "PG_ID_ASC",
	PG_ID_DESC = "PG_ID_DESC",
	PG_TYPE_ASC = "PG_TYPE_ASC",
	PG_TYPE_DESC = "PG_TYPE_DESC",
	PG_NAME_ASC = "PG_NAME_ASC",
	PG_NAME_DESC = "PG_NAME_DESC",
	PG_DESCRIPTION_ASC = "PG_DESCRIPTION_ASC",
	PG_DESCRIPTION_DESC = "PG_DESCRIPTION_DESC",
	PG_BASE_ASC = "PG_BASE_ASC",
	PG_BASE_DESC = "PG_BASE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PlatbyRaw`. */
export const enum PlatbyRawsOrderBy {
	NATURAL = "NATURAL",
	PR_ID_ASC = "PR_ID_ASC",
	PR_ID_DESC = "PR_ID_DESC",
	PR_RAW_ASC = "PR_RAW_ASC",
	PR_RAW_DESC = "PR_RAW_DESC",
	PR_HASH_ASC = "PR_HASH_ASC",
	PR_HASH_DESC = "PR_HASH_DESC",
	PR_SORTED_ASC = "PR_SORTED_ASC",
	PR_SORTED_DESC = "PR_SORTED_DESC",
	PR_DISCARDED_ASC = "PR_DISCARDED_ASC",
	PR_DISCARDED_DESC = "PR_DISCARDED_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Skupiny`. */
export const enum SkupiniesOrderBy {
	NATURAL = "NATURAL",
	S_ID_ASC = "S_ID_ASC",
	S_ID_DESC = "S_ID_DESC",
	S_NAME_ASC = "S_NAME_ASC",
	S_NAME_DESC = "S_NAME_DESC",
	S_DESCRIPTION_ASC = "S_DESCRIPTION_ASC",
	S_DESCRIPTION_DESC = "S_DESCRIPTION_DESC",
	S_COLOR_RGB_ASC = "S_COLOR_RGB_ASC",
	S_COLOR_RGB_DESC = "S_COLOR_RGB_DESC",
	S_COLOR_TEXT_ASC = "S_COLOR_TEXT_ASC",
	S_COLOR_TEXT_DESC = "S_COLOR_TEXT_DESC",
	S_LOCATION_ASC = "S_LOCATION_ASC",
	S_LOCATION_DESC = "S_LOCATION_DESC",
	S_VISIBLE_ASC = "S_VISIBLE_ASC",
	S_VISIBLE_DESC = "S_VISIBLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `UsersSkupiny`. */
export const enum UsersSkupiniesOrderBy {
	NATURAL = "NATURAL",
	US_ID_ASC = "US_ID_ASC",
	US_ID_DESC = "US_ID_DESC",
	US_COLOR_ASC = "US_COLOR_ASC",
	US_COLOR_DESC = "US_COLOR_DESC",
	US_PLATBA_MESIC_ASC = "US_PLATBA_MESIC_ASC",
	US_PLATBA_MESIC_DESC = "US_PLATBA_MESIC_DESC",
	US_PLATBA_CTVRTROK_ASC = "US_PLATBA_CTVRTROK_ASC",
	US_PLATBA_CTVRTROK_DESC = "US_PLATBA_CTVRTROK_DESC",
	US_PLATBA_PULROK_ASC = "US_PLATBA_PULROK_ASC",
	US_PLATBA_PULROK_DESC = "US_PLATBA_PULROK_DESC",
	US_POPIS_ASC = "US_POPIS_ASC",
	US_POPIS_DESC = "US_POPIS_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Video`. */
export const enum VideosOrderBy {
	NATURAL = "NATURAL",
	V_ID_ASC = "V_ID_ASC",
	V_ID_DESC = "V_ID_DESC",
	V_URI_ASC = "V_URI_ASC",
	V_URI_DESC = "V_URI_DESC",
	V_TITLE_ASC = "V_TITLE_ASC",
	V_TITLE_DESC = "V_TITLE_DESC",
	V_AUTHOR_ASC = "V_AUTHOR_ASC",
	V_AUTHOR_DESC = "V_AUTHOR_DESC",
	V_DESCRIPTION_ASC = "V_DESCRIPTION_ASC",
	V_DESCRIPTION_DESC = "V_DESCRIPTION_DESC",
	V_PLAYLIST_ASC = "V_PLAYLIST_ASC",
	V_PLAYLIST_DESC = "V_PLAYLIST_DESC",
	V_CREATED_AT_ASC = "V_CREATED_AT_ASC",
	V_CREATED_AT_DESC = "V_CREATED_AT_DESC",
	V_UPDATED_AT_ASC = "V_UPDATED_AT_ASC",
	V_UPDATED_AT_DESC = "V_UPDATED_AT_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `VideoList`. */
export const enum VideoListsOrderBy {
	NATURAL = "NATURAL",
	VL_ID_ASC = "VL_ID_ASC",
	VL_ID_DESC = "VL_ID_DESC",
	VL_URL_ASC = "VL_URL_ASC",
	VL_URL_DESC = "VL_URL_DESC",
	VL_TITLE_ASC = "VL_TITLE_ASC",
	VL_TITLE_DESC = "VL_TITLE_DESC",
	VL_DESCRIPTION_ASC = "VL_DESCRIPTION_ASC",
	VL_DESCRIPTION_DESC = "VL_DESCRIPTION_DESC",
	VL_COUNT_ASC = "VL_COUNT_ASC",
	VL_COUNT_DESC = "VL_COUNT_DESC",
	VL_CREATED_AT_ASC = "VL_CREATED_AT_ASC",
	VL_CREATED_AT_DESC = "VL_CREATED_AT_DESC",
	VL_LAST_CHECKED_ASC = "VL_LAST_CHECKED_ASC",
	VL_LAST_CHECKED_DESC = "VL_LAST_CHECKED_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `VideoSource`. */
export const enum VideoSourcesOrderBy {
	NATURAL = "NATURAL",
	VS_ID_ASC = "VS_ID_ASC",
	VS_ID_DESC = "VS_ID_DESC",
	VS_URL_ASC = "VS_URL_ASC",
	VS_URL_DESC = "VS_URL_DESC",
	VS_TITLE_ASC = "VS_TITLE_ASC",
	VS_TITLE_DESC = "VS_TITLE_DESC",
	VS_DESCRIPTION_ASC = "VS_DESCRIPTION_ASC",
	VS_DESCRIPTION_DESC = "VS_DESCRIPTION_DESC",
	VS_CREATED_AT_ASC = "VS_CREATED_AT_ASC",
	VS_CREATED_AT_DESC = "VS_CREATED_AT_DESC",
	VS_LAST_CHECKED_ASC = "VS_LAST_CHECKED_ASC",
	VS_LAST_CHECKED_DESC = "VS_LAST_CHECKED_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
export class GraphQLError extends Error {
    constructor(public response: GraphQLResponse) {
      super("");
      console.error(response);
    }
    toString() {
      return "GraphQL Response Error";
    }
  }


export type UnwrapPromise<T> = T extends Promise<infer R> ? R : T;
export type ZeusState<T extends (...args: any[]) => Promise<any>> = NonNullable<
  UnwrapPromise<ReturnType<T>>
>;
export type ZeusHook<
  T extends (
    ...args: any[]
  ) => Record<string, (...args: any[]) => Promise<any>>,
  N extends keyof ReturnType<T>
> = ZeusState<ReturnType<T>[N]>;

type WithTypeNameValue<T> = T & {
  __typename?: boolean;
};
type AliasType<T> = WithTypeNameValue<T> & {
  __alias?: Record<string, WithTypeNameValue<T>>;
};
export interface GraphQLResponse {
  data?: Record<string, any>;
  errors?: Array<{
    message: string;
  }>;
}
type DeepAnify<T> = {
  [P in keyof T]?: any;
};
type IsPayLoad<T> = T extends [any, infer PayLoad] ? PayLoad : T;
type IsArray<T, U> = T extends Array<infer R> ? InputType<R, U>[] : InputType<T, U>;
type FlattenArray<T> = T extends Array<infer R> ? R : T;

type IsInterfaced<SRC extends DeepAnify<DST>, DST> = FlattenArray<SRC> extends ZEUS_INTERFACES | ZEUS_UNIONS
  ? {
      [P in keyof SRC]: SRC[P] extends '__union' & infer R
        ? P extends keyof DST
          ? IsArray<R, '__typename' extends keyof DST ? DST[P] & { __typename: true } : DST[P]>
          : {}
        : never;
    }[keyof DST] &
      {
        [P in keyof Omit<
          Pick<
            SRC,
            {
              [P in keyof DST]: SRC[P] extends '__union' & infer R ? never : P;
            }[keyof DST]
          >,
          '__typename'
        >]: IsPayLoad<DST[P]> extends boolean ? SRC[P] : IsArray<SRC[P], DST[P]>;
      }
  : {
      [P in keyof Pick<SRC, keyof DST>]: IsPayLoad<DST[P]> extends boolean ? SRC[P] : IsArray<SRC[P], DST[P]>;
    };

export type MapType<SRC, DST> = SRC extends DeepAnify<DST> ? IsInterfaced<SRC, DST> : never;
export type InputType<SRC, DST> = IsPayLoad<DST> extends { __alias: infer R }
  ? {
      [P in keyof R]: MapType<SRC, R[P]>;
    } &
      MapType<SRC, Omit<IsPayLoad<DST>, '__alias'>>
  : MapType<SRC, IsPayLoad<DST>>;
type Func<P extends any[], R> = (...args: P) => R;
type AnyFunc = Func<any, any>;
export type ArgsType<F extends AnyFunc> = F extends Func<infer P, any> ? P : never;
export type OperationOptions = {
  variables?: Record<string, any>;
  operationName?: string;
};
export type SubscriptionToGraphQL<Z, T> = {
  ws: WebSocket;
  on: (fn: (args: InputType<T, Z>) => void) => void;
  off: (fn: (e: { data?: InputType<T, Z>; code?: number; reason?: string; message?: string }) => void) => void;
  error: (fn: (e: { data?: InputType<T, Z>; errors?: string[] }) => void) => void;
  open: () => void;
};
export type SelectionFunction<V> = <T>(t: T | V) => T;
export type fetchOptions = ArgsType<typeof fetch>;
type websocketOptions = typeof WebSocket extends new (
  ...args: infer R
) => WebSocket
  ? R
  : never;
export type chainOptions =
  | [fetchOptions[0], fetchOptions[1] & {websocket?: websocketOptions}]
  | [fetchOptions[0]];
export type FetchFunction = (
  query: string,
  variables?: Record<string, any>,
) => Promise<any>;
export type SubscriptionFunction = (query: string) => any;
type NotUndefined<T> = T extends undefined ? never : T;
export type ResolverType<F> = NotUndefined<F extends [infer ARGS, any] ? ARGS : undefined>;



export const ZeusSelect = <T>() => ((t: any) => t) as SelectionFunction<T>;

export const ScalarResolver = (scalar: string, value: any) => {
  switch (scalar) {
    case 'String':
      return  `${JSON.stringify(value)}`;
    case 'Int':
      return `${value}`;
    case 'Float':
      return `${value}`;
    case 'Boolean':
      return `${value}`;
    case 'ID':
      return `"${value}"`;
    case 'enum':
      return `${value}`;
    case 'scalar':
      return `${value}`;
    default:
      return false;
  }
};


export const TypesPropsResolver = ({
    value,
    type,
    name,
    key,
    blockArrays
}: {
    value: any;
    type: string;
    name: string;
    key?: string;
    blockArrays?: boolean;
}): string => {
    if (value === null) {
        return `null`;
    }
    let resolvedValue = AllTypesProps[type][name];
    if (key) {
        resolvedValue = resolvedValue[key];
    }
    if (!resolvedValue) {
        throw new Error(`Cannot resolve ${type} ${name}${key ? ` ${key}` : ''}`)
    }
    const typeResolved = resolvedValue.type;
    const isArray = resolvedValue.array;
    const isArrayRequired = resolvedValue.arrayRequired;
    if (typeof value === 'string' && value.startsWith(`ZEUS_VAR$`)) {
        const isRequired = resolvedValue.required ? '!' : '';
        let t = `${typeResolved}`;
        if (isArray) {
          if (isRequired) {
              t = `${t}!`;
          }
          t = `[${t}]`;
          if(isArrayRequired){
            t = `${t}!`;
          }
        }else{
          if (isRequired) {
                t = `${t}!`;
          }
        }
        return `\$${value.split(`ZEUS_VAR$`)[1]}__ZEUS_VAR__${t}`;
    }
    if (isArray && !blockArrays) {
        return `[${value
        .map((v: any) => TypesPropsResolver({ value: v, type, name, key, blockArrays: true }))
        .join(',')}]`;
    }
    const reslovedScalar = ScalarResolver(typeResolved, value);
    if (!reslovedScalar) {
        const resolvedType = AllTypesProps[typeResolved];
        if (typeof resolvedType === 'object') {
        const argsKeys = Object.keys(resolvedType);
        return `{${argsKeys
            .filter((ak) => value[ak] !== undefined)
            .map(
            (ak) => `${ak}:${TypesPropsResolver({ value: value[ak], type: typeResolved, name: ak })}`
            )}}`;
        }
        return ScalarResolver(AllTypesProps[typeResolved], value) as string;
    }
    return reslovedScalar;
};


const isArrayFunction = (
  parent: string[],
  a: any[]
) => {
  const [values, r] = a;
  const [mainKey, key, ...keys] = parent;
  const keyValues = Object.keys(values).filter((k) => typeof values[k] !== 'undefined');

  if (!keys.length) {
      return keyValues.length > 0
        ? `(${keyValues
            .map(
              (v) =>
                `${v}:${TypesPropsResolver({
                  value: values[v],
                  type: mainKey,
                  name: key,
                  key: v
                })}`
            )
            .join(',')})${r ? traverseToSeekArrays(parent, r) : ''}`
        : traverseToSeekArrays(parent, r);
    }

  const [typeResolverKey] = keys.splice(keys.length - 1, 1);
  let valueToResolve = ReturnTypes[mainKey][key];
  for (const k of keys) {
    valueToResolve = ReturnTypes[valueToResolve][k];
  }

  const argumentString =
    keyValues.length > 0
      ? `(${keyValues
          .map(
            (v) =>
              `${v}:${TypesPropsResolver({
                value: values[v],
                type: valueToResolve,
                name: typeResolverKey,
                key: v
              })}`
          )
          .join(',')})${r ? traverseToSeekArrays(parent, r) : ''}`
      : traverseToSeekArrays(parent, r);
  return argumentString;
};


const resolveKV = (k: string, v: boolean | string | { [x: string]: boolean | string }) =>
  typeof v === 'boolean' ? k : typeof v === 'object' ? `${k}{${objectToTree(v)}}` : `${k}${v}`;


const objectToTree = (o: { [x: string]: boolean | string }): string =>
  `{${Object.keys(o).map((k) => `${resolveKV(k, o[k])}`).join(' ')}}`;


const traverseToSeekArrays = (parent: string[], a?: any): string => {
  if (!a) return '';
  if (Object.keys(a).length === 0) {
    return '';
  }
  let b: Record<string, any> = {};
  if (Array.isArray(a)) {
    return isArrayFunction([...parent], a);
  } else {
    if (typeof a === 'object') {
      Object.keys(a)
        .filter((k) => typeof a[k] !== 'undefined')
        .forEach((k) => {
        if (k === '__alias') {
          Object.keys(a[k]).forEach((aliasKey) => {
            const aliasOperations = a[k][aliasKey];
            const aliasOperationName = Object.keys(aliasOperations)[0];
            const aliasOperation = aliasOperations[aliasOperationName];
            b[
              `${aliasOperationName}__alias__${aliasKey}: ${aliasOperationName}`
            ] = traverseToSeekArrays([...parent, aliasOperationName], aliasOperation);
          });
        } else {
          b[k] = traverseToSeekArrays([...parent, k], a[k]);
        }
      });
    } else {
      return '';
    }
  }
  return objectToTree(b);
};  


const buildQuery = (type: string, a?: Record<any, any>) => 
  traverseToSeekArrays([type], a);


const inspectVariables = (query: string) => {
  const regex = /\$\b\w*__ZEUS_VAR__\[?[^!^\]^\s^,^\)^\}]*[!]?[\]]?[!]?/g;
  let result;
  const AllVariables: string[] = [];
  while ((result = regex.exec(query))) {
    if (AllVariables.includes(result[0])) {
      continue;
    }
    AllVariables.push(result[0]);
  }
  if (!AllVariables.length) {
    return query;
  }
  let filteredQuery = query;
  AllVariables.forEach((variable) => {
    while (filteredQuery.includes(variable)) {
      filteredQuery = filteredQuery.replace(variable, variable.split('__ZEUS_VAR__')[0]);
    }
  });
  return `(${AllVariables.map((a) => a.split('__ZEUS_VAR__'))
    .map(([variableName, variableType]) => `${variableName}:${variableType}`)
    .join(', ')})${filteredQuery}`;
};


export const queryConstruct = (t: 'query' | 'mutation' | 'subscription', tName: string, operationName?: string) => (o: Record<any, any>) =>
  `${t.toLowerCase()}${operationName ? ' ' + operationName : ''}${inspectVariables(buildQuery(tName, o))}`;
  

export const fullChainConstruct = (fn: FetchFunction) => (t: 'query' | 'mutation' | 'subscription', tName: string) => (
  o: Record<any, any>,
  options?: OperationOptions,
) => fn(queryConstruct(t, tName, options?.operationName)(o), options?.variables).then((r:any) => { 
  seekForAliases(r)
  return r
});


export const fullSubscriptionConstruct = (fn: SubscriptionFunction) => (
  t: 'query' | 'mutation' | 'subscription',
  tName: string,
) => (o: Record<any, any>, options?: OperationOptions) =>
  fn(queryConstruct(t, tName, options?.operationName)(o));


const seekForAliases = (response: any) => {
  const traverseAlias = (value: any) => {
    if (Array.isArray(value)) {
      value.forEach(seekForAliases);
    } else {
      if (typeof value === 'object') {
        seekForAliases(value);
      }
    }
  };
  if (typeof response === 'object' && response) {
    const keys = Object.keys(response);
    if (keys.length < 1) {
      return;
    }
    keys.forEach((k) => {
      const value = response[k];
      if (k.indexOf('__alias__') !== -1) {
        const [operation, alias] = k.split('__alias__');
        response[alias] = {
          [operation]: value,
        };
        delete response[k];
      }
      traverseAlias(value);
    });
  }
};


export const $ = (t: TemplateStringsArray): any => `ZEUS_VAR$${t.join('')}`;


export const resolverFor = <
  X,
  T extends keyof ValueTypes,
  Z extends keyof ValueTypes[T],
>(
  type: T,
  field: Z,
  fn: (
    args: Required<ValueTypes[T]>[Z] extends [infer Input, any] ? Input : any,
    source: any,
  ) => Z extends keyof ModelTypes[T] ? ModelTypes[T][Z] | Promise<ModelTypes[T][Z]> | X : any,
) => fn as (args?: any,source?: any) => any;


const handleFetchResponse = (
  response: Parameters<Extract<Parameters<ReturnType<typeof fetch>['then']>[0], Function>>[0]
): Promise<GraphQLResponse> => {
  if (!response.ok) {
    return new Promise((_, reject) => {
      response.text().then(text => {
        try { reject(JSON.parse(text)); }
        catch (err) { reject(text); }
      }).catch(reject);
    });
  }
  return response.json();
};

export const apiFetch = (options: fetchOptions) => (query: string, variables: Record<string, any> = {}) => {
    let fetchFunction = fetch;
    let queryString = query;
    let fetchOptions = options[1] || {};
    if (fetchOptions.method && fetchOptions.method === 'GET') {
      queryString = encodeURIComponent(query);
      return fetchFunction(`${options[0]}?query=${queryString}`, fetchOptions)
        .then(handleFetchResponse)
        .then((response: GraphQLResponse) => {
          if (response.errors) {
            throw new GraphQLError(response);
          }
          return response.data;
        });
    }
    return fetchFunction(`${options[0]}`, {
      body: JSON.stringify({ query: queryString, variables }),
      method: 'POST',
      headers: {
        'Content-Type': 'application/json'
      },
      ...fetchOptions
    })
      .then(handleFetchResponse)
      .then((response: GraphQLResponse) => {
        if (response.errors) {
          throw new GraphQLError(response);
        }
        return response.data;
      });
  };
  

export const apiSubscription = (options: chainOptions) => (
    query: string,
  ) => {
    try {
      const queryString = options[0] + '?query=' + encodeURIComponent(query);
      const wsString = queryString.replace('http', 'ws');
      const host = (options.length > 1 && options[1]?.websocket?.[0]) || wsString;
      const webSocketOptions = options[1]?.websocket || [host];
      const ws = new WebSocket(...webSocketOptions);
      return {
        ws,
        on: (e: (args: any) => void) => {
          ws.onmessage = (event:any) => {
            if(event.data){
              const parsed = JSON.parse(event.data)
              const data = parsed.data
              if (data) {
                seekForAliases(data);
              }
              return e(data);
            }
          };
        },
        off: (e: (args: any) => void) => {
          ws.onclose = e;
        },
        error: (e: (args: any) => void) => {
          ws.onerror = e;
        },
        open: (e: () => void) => {
          ws.onopen = e;
        },
      };
    } catch {
      throw new Error('No websockets implemented');
    }
  };



const allOperations = {
    "query": "Query",
    "mutation": "Mutation"
}

export type GenericOperation<O> = O extends 'query'
  ? "Query"
  : O extends 'mutation'
  ? "Mutation"
  : never

export const Thunder = (fn: FetchFunction) => <
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
) => <Z extends ValueTypes[R]>(o: Z | ValueTypes[R], ops?: OperationOptions) =>
  fullChainConstruct(fn)(operation, allOperations[operation])(o as any, ops) as Promise<InputType<GraphQLTypes[R], Z>>;

export const Chain = (...options: chainOptions) => Thunder(apiFetch(options));  
  
export const SubscriptionThunder = (fn: SubscriptionFunction) => <
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
) => <Z extends ValueTypes[R]>(
  o: Z | ValueTypes[R],
  ops?: OperationOptions
)=>
  fullSubscriptionConstruct(fn)(operation, allOperations[operation])(
    o as any,
    ops,
  ) as SubscriptionToGraphQL<Z, GraphQLTypes[R]>;

export const Subscription = (...options: chainOptions) => SubscriptionThunder(apiSubscription(options));
export const Zeus = <
  Z extends ValueTypes[R],
  O extends 'query' | 'mutation',
  R extends keyof ValueTypes = GenericOperation<O>
>(
  operation: O,
  o: Z | ValueTypes[R],
  operationName?: string,
) => queryConstruct(operation, allOperations[operation], operationName)(o as any);
export const Selector = <T extends keyof ValueTypes>(key: T) => ZeusSelect<ValueTypes[T]>();
  

export const Gql = Chain('http://localhost:3000/graphql')