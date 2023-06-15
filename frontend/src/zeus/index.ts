/* eslint-disable */

import { AllTypesProps, ReturnTypes } from './const';
type ZEUS_INTERFACES = never
type ZEUS_UNIONS = never

export type ValueTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: AliasType<{
	/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query?:ValueTypes["Query"],
akces?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Akce`. */
	orderBy?:ValueTypes["AkcesOrderBy"][]},ValueTypes["AkcesConnection"]],
akceItems?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsConnection"]],
aktualities?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AktualityCondition"] | null},ValueTypes["AktualitiesConnection"]],
attachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Attachment`. */
	orderBy?:ValueTypes["AttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttachmentCondition"] | null},ValueTypes["AttachmentsConnection"]],
attendeeExternals?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeExternalCondition"] | null},ValueTypes["AttendeeExternalsConnection"]],
attendeeUsers?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeUserCondition"] | null},ValueTypes["AttendeeUsersConnection"]],
cohortGroups?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `CohortGroup`. */
	orderBy?:ValueTypes["CohortGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["CohortGroupCondition"] | null},ValueTypes["CohortGroupsConnection"]],
dokumenties?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Dokumenty`. */
	orderBy?:ValueTypes["DokumentiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["DokumentyCondition"] | null},ValueTypes["DokumentiesConnection"]],
events?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Event`. */
	orderBy?:ValueTypes["EventsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["EventCondition"] | null},ValueTypes["EventsConnection"]],
formResponses?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `FormResponse`. */
	orderBy?:ValueTypes["FormResponsesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["FormResponseCondition"] | null},ValueTypes["FormResponsesConnection"]],
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
locations?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Location`. */
	orderBy?:ValueTypes["LocationsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["LocationCondition"] | null},ValueTypes["LocationsConnection"]],
locationAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["LocationAttachmentCondition"] | null},ValueTypes["LocationAttachmentsConnection"]],
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
people?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Person`. */
	orderBy?:ValueTypes["PeopleOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["PersonCondition"] | null},ValueTypes["PeopleConnection"]],
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
rooms?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Room`. */
	orderBy?:ValueTypes["RoomsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RoomCondition"] | null},ValueTypes["RoomsConnection"]],
roomAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RoomAttachmentCondition"] | null},ValueTypes["RoomAttachmentsConnection"]],
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
tenants?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Tenant`. */
	orderBy?:ValueTypes["TenantsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantCondition"] | null},ValueTypes["TenantsConnection"]],
tenantAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantAttachmentCondition"] | null},ValueTypes["TenantAttachmentsConnection"]],
tenantLocations?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantLocationCondition"] | null},ValueTypes["TenantLocationsConnection"]],
tenantPeople?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantPersonCondition"] | null},ValueTypes["TenantPeopleConnection"]],
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
aktuality?: [{	atId:ValueTypes["BigInt"]},ValueTypes["Aktuality"]],
attachment?: [{	objectName:string},ValueTypes["Attachment"]],
attendeeExternal?: [{	id:ValueTypes["BigInt"]},ValueTypes["AttendeeExternal"]],
attendeeUser?: [{	id:ValueTypes["BigInt"]},ValueTypes["AttendeeUser"]],
attendeeUserByUserIdAndEventId?: [{	userId:ValueTypes["BigInt"],	eventId:ValueTypes["BigInt"]},ValueTypes["AttendeeUser"]],
cohortGroup?: [{	id:ValueTypes["BigInt"]},ValueTypes["CohortGroup"]],
dokumenty?: [{	dId:ValueTypes["BigInt"]},ValueTypes["Dokumenty"]],
event?: [{	id:ValueTypes["BigInt"]},ValueTypes["Event"]],
formResponse?: [{	id:ValueTypes["BigInt"]},ValueTypes["FormResponse"]],
galerieDir?: [{	gdId:ValueTypes["BigInt"]},ValueTypes["GalerieDir"]],
galerieFoto?: [{	gfId:ValueTypes["BigInt"]},ValueTypes["GalerieFoto"]],
location?: [{	id:ValueTypes["BigInt"]},ValueTypes["Location"]],
locationAttachment?: [{	locationId:ValueTypes["BigInt"],	objectName:string},ValueTypes["LocationAttachment"]],
nabidka?: [{	nId:ValueTypes["BigInt"]},ValueTypes["Nabidka"]],
nabidkaItem?: [{	niId:ValueTypes["BigInt"]},ValueTypes["NabidkaItem"]],
nabidkaItemByNiPartnerAndNiIdRodic?: [{	niPartner:ValueTypes["BigInt"],	niIdRodic:ValueTypes["BigInt"]},ValueTypes["NabidkaItem"]],
page?: [{	id:number},ValueTypes["Page"]],
pageByUrl?: [{	url:string},ValueTypes["Page"]],
pageRevision?: [{	revNumber:number,	id:number},ValueTypes["PageRevision"]],
parameter?: [{	paName:string},ValueTypes["Parameter"]],
pary?: [{	pId:ValueTypes["BigInt"]},ValueTypes["Pary"]],
paryNavrh?: [{	pnId:ValueTypes["BigInt"]},ValueTypes["ParyNavrh"]],
permission?: [{	peId:ValueTypes["BigInt"]},ValueTypes["Permission"]],
person?: [{	id:ValueTypes["BigInt"]},ValueTypes["Person"]],
platbyCategory?: [{	pcId:ValueTypes["BigInt"]},ValueTypes["PlatbyCategory"]],
platbyCategoryGroup?: [{	pcgId:ValueTypes["BigInt"]},ValueTypes["PlatbyCategoryGroup"]],
platbyGroup?: [{	pgId:ValueTypes["BigInt"]},ValueTypes["PlatbyGroup"]],
platbyGroupSkupina?: [{	pgsId:ValueTypes["BigInt"]},ValueTypes["PlatbyGroupSkupina"]],
platbyItem?: [{	piId:ValueTypes["BigInt"]},ValueTypes["PlatbyItem"]],
platbyRaw?: [{	prId:ValueTypes["BigInt"]},ValueTypes["PlatbyRaw"]],
room?: [{	id:ValueTypes["BigInt"]},ValueTypes["Room"]],
roomAttachment?: [{	roomId:ValueTypes["BigInt"],	objectName:string},ValueTypes["RoomAttachment"]],
rozpi?: [{	rId:ValueTypes["BigInt"]},ValueTypes["Rozpi"]],
rozpisItem?: [{	riId:ValueTypes["BigInt"]},ValueTypes["RozpisItem"]],
session?: [{	ssId:string},ValueTypes["Session"]],
skupiny?: [{	sId:ValueTypes["BigInt"]},ValueTypes["Skupiny"]],
tenant?: [{	id:ValueTypes["BigInt"]},ValueTypes["Tenant"]],
tenantAttachment?: [{	tenantId:ValueTypes["BigInt"],	objectName:string},ValueTypes["TenantAttachment"]],
tenantLocation?: [{	tenantId:ValueTypes["BigInt"],	locationId:ValueTypes["BigInt"]},ValueTypes["TenantLocation"]],
tenantPerson?: [{	tenantId:ValueTypes["BigInt"],	personId:ValueTypes["BigInt"]},ValueTypes["TenantPerson"]],
upozorneni?: [{	upId:ValueTypes["BigInt"]},ValueTypes["Upozorneni"]],
upozorneniSkupiny?: [{	upsId:ValueTypes["BigInt"]},ValueTypes["UpozorneniSkupiny"]],
user?: [{	uId:ValueTypes["BigInt"]},ValueTypes["User"]],
activeCouples?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["PariesConnection"]],
currentCoupleIds?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["CurrentCoupleIdsConnection"]],
currentPermissions?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["PermissionsConnection"]],
	currentSessionId?:boolean,
	currentTenantId?:boolean,
	currentUserId?:boolean,
	getCurrentCouple?:ValueTypes["Pary"],
	getCurrentTenant?:ValueTypes["Tenant"],
	getCurrentUser?:ValueTypes["User"],
myAnnouncements?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["UpozornenisConnection"]],
myLessons?: [{	startDate?:ValueTypes["Date"] | null,	endDate?:ValueTypes["Date"] | null,	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["RozpisItemsConnection"]],
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
trainers?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null},ValueTypes["UsersConnection"]],
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
	summary?:boolean,
	isPublic?:boolean,
	enableNotes?:boolean,
akceItemsByAiIdRodic?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsConnection"]],
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
	aiId?:boolean,
	aiIdRodic?:boolean,
	aiUser?:boolean,
	aiRokNarozeni?:boolean,
	notes?:boolean,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ValueTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ValueTypes["User"],
		__typename?: boolean
}>;
	["User"]: AliasType<{
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
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ValueTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ValueTypes["Skupiny"],
attendeeUsers?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeUserCondition"] | null},ValueTypes["AttendeeUsersConnection"]],
aktualitiesByAtKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Aktuality`. */
	orderBy?:ValueTypes["AktualitiesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AktualityCondition"] | null},ValueTypes["AktualitiesConnection"]],
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
nabidkasByNTrener?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["NabidkaCondition"] | null},ValueTypes["NabidkasConnection"]],
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
upozornenisByUpKdo?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Upozorneni`. */
	orderBy?:ValueTypes["UpozornenisOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["UpozorneniCondition"] | null},ValueTypes["UpozornenisConnection"]],
attachmentsByUploadedBy?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Attachment`. */
	orderBy?:ValueTypes["AttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttachmentCondition"] | null},ValueTypes["AttachmentsConnection"]],
attendeeExternalsByManagedBy?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeExternalCondition"] | null},ValueTypes["AttendeeExternalsConnection"]],
attendeeExternalsByConfirmedBy?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeExternalCondition"] | null},ValueTypes["AttendeeExternalsConnection"]],
akceItemsByAiUser?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AkceItem`. */
	orderBy?:ValueTypes["AkceItemsOrderBy"][]},ValueTypes["AkceItemsConnection"]],
pariesByPIdPartnerka?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["ParyCondition"] | null},ValueTypes["PariesConnection"]],
	dateOfNewestPayment?:boolean,
	dateOfOldestPayment?:boolean,
	fullName?:boolean,
	hasValidPayment?:boolean,
	inPublicCohort?:boolean,
		__typename?: boolean
}>;
	["Permission"]: AliasType<{
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
	id?:boolean,
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
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?:string | null,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?:string | null,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `uBan` field. */
	uBan?:boolean | null,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?:boolean | null,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?:boolean | null,
	/** Checks for equality with the object’s `inPublicCohort` field. */
	inPublicCohort?:boolean | null
};
	["Skupiny"]: AliasType<{
	sId?:boolean,
	sName?:boolean,
	sDescription?:boolean,
	sColorRgb?:boolean,
	sColorText?:boolean,
	sLocation?:boolean,
	sVisible?:boolean,
	ordering?:boolean,
	internalInfo?:boolean,
	cohortGroup?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ValueTypes["CohortGroup"],
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
	["CohortGroup"]: AliasType<{
	id?:boolean,
	name?:boolean,
	description?:boolean,
	ordering?:boolean,
	isPublic?:boolean,
	tenant?:boolean,
	tenantId?:boolean,
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ValueTypes["Tenant"],
skupiniesByCohortGroup?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["SkupinyCondition"] | null},ValueTypes["SkupiniesConnection"]],
		__typename?: boolean
}>;
	["Tenant"]: AliasType<{
	id?:boolean,
	name?:boolean,
	memberInfo?:boolean,
	origins?:boolean,
tenantAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantAttachmentCondition"] | null},ValueTypes["TenantAttachmentsConnection"]],
tenantPeople?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantPersonCondition"] | null},ValueTypes["TenantPeopleConnection"]],
cohortGroupsByTenant?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `CohortGroup`. */
	orderBy?:ValueTypes["CohortGroupsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["CohortGroupCondition"] | null},ValueTypes["CohortGroupsConnection"]],
tenantLocations?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantLocationCondition"] | null},ValueTypes["TenantLocationsConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `TenantAttachment` values. */
["TenantAttachmentsConnection"]: AliasType<{
	/** A list of `TenantAttachment` objects. */
	nodes?:ValueTypes["TenantAttachment"],
	/** A list of edges which contains the `TenantAttachment` and cursor to aid in pagination. */
	edges?:ValueTypes["TenantAttachmentsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `TenantAttachment` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["TenantAttachment"]: AliasType<{
	tenantId?:boolean,
	objectName?:boolean,
	type?:boolean,
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
		__typename?: boolean
}>;
	["TenantAttachmentType"]:TenantAttachmentType;
	["Attachment"]: AliasType<{
	objectName?:boolean,
	previewObjectName?:boolean,
	uploadedBy?:boolean,
	uploadedAt?:boolean,
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ValueTypes["User"],
tenantAttachmentsByObjectName?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantAttachmentCondition"] | null},ValueTypes["TenantAttachmentsConnection"]],
locationAttachmentsByObjectName?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["LocationAttachmentCondition"] | null},ValueTypes["LocationAttachmentsConnection"]],
roomAttachmentsByObjectName?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RoomAttachmentCondition"] | null},ValueTypes["RoomAttachmentsConnection"]],
		__typename?: boolean
}>;
	/** Methods to use when ordering `TenantAttachment`. */
["TenantAttachmentsOrderBy"]:TenantAttachmentsOrderBy;
	/** A condition to be used against `TenantAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantAttachmentCondition"]: {
	/** Checks for equality with the object’s `tenantId` field. */
	tenantId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `objectName` field. */
	objectName?:string | null
};
	/** A connection to a list of `LocationAttachment` values. */
["LocationAttachmentsConnection"]: AliasType<{
	/** A list of `LocationAttachment` objects. */
	nodes?:ValueTypes["LocationAttachment"],
	/** A list of edges which contains the `LocationAttachment` and cursor to aid in pagination. */
	edges?:ValueTypes["LocationAttachmentsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `LocationAttachment` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["LocationAttachment"]: AliasType<{
	locationId?:boolean,
	objectName?:boolean,
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ValueTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
		__typename?: boolean
}>;
	["Location"]: AliasType<{
	id?:boolean,
	name?:boolean,
	description?:boolean,
roomsByLocation?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `Room`. */
	orderBy?:ValueTypes["RoomsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RoomCondition"] | null},ValueTypes["RoomsConnection"]],
locationAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["LocationAttachmentCondition"] | null},ValueTypes["LocationAttachmentsConnection"]],
tenantLocations?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantLocationCondition"] | null},ValueTypes["TenantLocationsConnection"]],
		__typename?: boolean
}>;
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:unknown;
	/** A connection to a list of `Room` values. */
["RoomsConnection"]: AliasType<{
	/** A list of `Room` objects. */
	nodes?:ValueTypes["Room"],
	/** A list of edges which contains the `Room` and cursor to aid in pagination. */
	edges?:ValueTypes["RoomsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Room` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["Room"]: AliasType<{
	id?:boolean,
	name?:boolean,
	description?:boolean,
	location?:boolean,
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ValueTypes["Location"],
roomAttachments?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["RoomAttachmentCondition"] | null},ValueTypes["RoomAttachmentsConnection"]],
		__typename?: boolean
}>;
	/** A connection to a list of `RoomAttachment` values. */
["RoomAttachmentsConnection"]: AliasType<{
	/** A list of `RoomAttachment` objects. */
	nodes?:ValueTypes["RoomAttachment"],
	/** A list of edges which contains the `RoomAttachment` and cursor to aid in pagination. */
	edges?:ValueTypes["RoomAttachmentsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `RoomAttachment` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["RoomAttachment"]: AliasType<{
	roomId?:boolean,
	objectName?:boolean,
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ValueTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
		__typename?: boolean
}>;
	/** A `RoomAttachment` edge in the connection. */
["RoomAttachmentsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `RoomAttachment` at the end of the edge. */
	node?:ValueTypes["RoomAttachment"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `RoomAttachment`. */
["RoomAttachmentsOrderBy"]:RoomAttachmentsOrderBy;
	/** A condition to be used against `RoomAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["RoomAttachmentCondition"]: {
	/** Checks for equality with the object’s `roomId` field. */
	roomId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `objectName` field. */
	objectName?:string | null
};
	/** A `Room` edge in the connection. */
["RoomsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Room` at the end of the edge. */
	node?:ValueTypes["Room"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Room`. */
["RoomsOrderBy"]:RoomsOrderBy;
	/** A condition to be used against `Room` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RoomCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `location` field. */
	location?:ValueTypes["BigInt"] | null
};
	/** Methods to use when ordering `LocationAttachment`. */
["LocationAttachmentsOrderBy"]:LocationAttachmentsOrderBy;
	/** A condition to be used against `LocationAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["LocationAttachmentCondition"]: {
	/** Checks for equality with the object’s `locationId` field. */
	locationId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `objectName` field. */
	objectName?:string | null
};
	/** A connection to a list of `TenantLocation` values. */
["TenantLocationsConnection"]: AliasType<{
	/** A list of `TenantLocation` objects. */
	nodes?:ValueTypes["TenantLocation"],
	/** A list of edges which contains the `TenantLocation` and cursor to aid in pagination. */
	edges?:ValueTypes["TenantLocationsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `TenantLocation` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["TenantLocation"]: AliasType<{
	tenantId?:boolean,
	locationId?:boolean,
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ValueTypes["Location"],
		__typename?: boolean
}>;
	/** A `TenantLocation` edge in the connection. */
["TenantLocationsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `TenantLocation` at the end of the edge. */
	node?:ValueTypes["TenantLocation"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `TenantLocation`. */
["TenantLocationsOrderBy"]:TenantLocationsOrderBy;
	/** A condition to be used against `TenantLocation` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantLocationCondition"]: {
	/** Checks for equality with the object’s `tenantId` field. */
	tenantId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `locationId` field. */
	locationId?:ValueTypes["BigInt"] | null
};
	/** A `LocationAttachment` edge in the connection. */
["LocationAttachmentsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `LocationAttachment` at the end of the edge. */
	node?:ValueTypes["LocationAttachment"],
		__typename?: boolean
}>;
	/** A `TenantAttachment` edge in the connection. */
["TenantAttachmentsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `TenantAttachment` at the end of the edge. */
	node?:ValueTypes["TenantAttachment"],
		__typename?: boolean
}>;
	/** A connection to a list of `TenantPerson` values. */
["TenantPeopleConnection"]: AliasType<{
	/** A list of `TenantPerson` objects. */
	nodes?:ValueTypes["TenantPerson"],
	/** A list of edges which contains the `TenantPerson` and cursor to aid in pagination. */
	edges?:ValueTypes["TenantPeopleEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `TenantPerson` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["TenantPerson"]: AliasType<{
	tenantId?:boolean,
	personId?:boolean,
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ValueTypes["Person"],
		__typename?: boolean
}>;
	["Person"]: AliasType<{
	id?:boolean,
	firstName?:boolean,
	lastName?:boolean,
	gender?:boolean,
tenantPeople?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["TenantPersonCondition"] | null},ValueTypes["TenantPeopleConnection"]],
		__typename?: boolean
}>;
	["GenderType"]:GenderType;
	/** Methods to use when ordering `TenantPerson`. */
["TenantPeopleOrderBy"]:TenantPeopleOrderBy;
	/** A condition to be used against `TenantPerson` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantPersonCondition"]: {
	/** Checks for equality with the object’s `tenantId` field. */
	tenantId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `personId` field. */
	personId?:ValueTypes["BigInt"] | null
};
	/** A `TenantPerson` edge in the connection. */
["TenantPeopleEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `TenantPerson` at the end of the edge. */
	node?:ValueTypes["TenantPerson"],
		__typename?: boolean
}>;
	/** A connection to a list of `CohortGroup` values. */
["CohortGroupsConnection"]: AliasType<{
	/** A list of `CohortGroup` objects. */
	nodes?:ValueTypes["CohortGroup"],
	/** A list of edges which contains the `CohortGroup` and cursor to aid in pagination. */
	edges?:ValueTypes["CohortGroupsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `CohortGroup` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `CohortGroup` edge in the connection. */
["CohortGroupsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `CohortGroup` at the end of the edge. */
	node?:ValueTypes["CohortGroup"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `CohortGroup`. */
["CohortGroupsOrderBy"]:CohortGroupsOrderBy;
	/** A condition to be used against `CohortGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["CohortGroupCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `ordering` field. */
	ordering?:number | null,
	/** Checks for equality with the object’s `isPublic` field. */
	isPublic?:boolean | null,
	/** Checks for equality with the object’s `tenant` field. */
	tenant?:ValueTypes["BigInt"] | null
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
	/** Checks for equality with the object’s `sVisible` field. */
	sVisible?:boolean | null,
	/** Checks for equality with the object’s `ordering` field. */
	ordering?:number | null,
	/** Checks for equality with the object’s `cohortGroup` field. */
	cohortGroup?:ValueTypes["BigInt"] | null
};
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
	pgsId?:boolean,
	pgsIdSkupina?:boolean,
	pgsIdGroup?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ValueTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ValueTypes["PlatbyGroup"],
		__typename?: boolean
}>;
	["PlatbyGroup"]: AliasType<{
	pgId?:boolean,
	pgType?:boolean,
	pgName?:boolean,
	pgDescription?:boolean,
	pgBase?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	pcgId?:boolean,
	pcgIdGroup?:boolean,
	pcgIdCategory?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ValueTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ValueTypes["PlatbyCategory"],
		__typename?: boolean
}>;
	["PlatbyCategory"]: AliasType<{
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
	id?:boolean,
	tenantId?:boolean,
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
	piId?:boolean,
	piIdUser?:boolean,
	piIdCategory?:boolean,
	piIdRaw?:boolean,
	piAmount?:boolean,
	piDate?:boolean,
	piPrefix?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ValueTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ValueTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ValueTypes["PlatbyRaw"],
		__typename?: boolean
}>;
	["PlatbyRaw"]: AliasType<{
	prId?:boolean,
	prRaw?:boolean,
	prHash?:boolean,
	prSorted?:boolean,
	prDiscarded?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	piIdRaw?:ValueTypes["BigInt"] | null
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
	upsId?:boolean,
	upsIdRodic?:boolean,
	upsIdSkupina?:boolean,
	upsColor?:boolean,
	upsPopis?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ValueTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ValueTypes["Skupiny"],
		__typename?: boolean
}>;
	["Upozorneni"]: AliasType<{
	upId?:boolean,
	upKdo?:boolean,
	upNadpis?:boolean,
	upText?:boolean,
	upBarvy?:boolean,
	upLock?:boolean,
	upTimestamp?:boolean,
	upTimestampAdd?:boolean,
	scheduledSince?:boolean,
	scheduledUntil?:boolean,
	isVisible?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	upsIdSkupina?:ValueTypes["BigInt"] | null
};
	/** A `UpozorneniSkupiny` edge in the connection. */
["UpozorneniSkupiniesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `UpozorneniSkupiny` at the end of the edge. */
	node?:ValueTypes["UpozorneniSkupiny"],
		__typename?: boolean
}>;
	/** A connection to a list of `AttendeeUser` values. */
["AttendeeUsersConnection"]: AliasType<{
	/** A list of `AttendeeUser` objects. */
	nodes?:ValueTypes["AttendeeUser"],
	/** A list of edges which contains the `AttendeeUser` and cursor to aid in pagination. */
	edges?:ValueTypes["AttendeeUsersEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `AttendeeUser` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["AttendeeUser"]: AliasType<{
	id?:boolean,
	eventId?:boolean,
	userId?:boolean,
	birthYear?:boolean,
	notes?:boolean,
	tenantId?:boolean,
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ValueTypes["User"],
		__typename?: boolean
}>;
	["Event"]: AliasType<{
	id?:boolean,
	name?:boolean,
	locationText?:boolean,
	description?:boolean,
	since?:boolean,
	until?:boolean,
	capacity?:boolean,
	filesLegacy?:boolean,
	updatedAt?:boolean,
	isLocked?:boolean,
	isVisible?:boolean,
	summary?:boolean,
	isPublic?:boolean,
	enableNotes?:boolean,
	tenantId?:boolean,
attendeeUsers?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeUserCondition"] | null},ValueTypes["AttendeeUsersConnection"]],
attendeeExternals?: [{	/** Only read the first `n` values of the set. */
	first?:number | null,	/** Only read the last `n` values of the set. */
	last?:number | null,	/** Skip the first `n` values from our `after` cursor, an alternative to cursor
based pagination. May not be used with `last`. */
	offset?:number | null,	/** Read all values in the set before (above) this cursor. */
	before?:ValueTypes["Cursor"] | null,	/** Read all values in the set after (below) this cursor. */
	after?:ValueTypes["Cursor"] | null,	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][],	/** A condition to be used in determining which values should be returned by the collection. */
	condition?:ValueTypes["AttendeeExternalCondition"] | null},ValueTypes["AttendeeExternalsConnection"]],
	remainingSpots?:boolean,
		__typename?: boolean
}>;
	/** Methods to use when ordering `AttendeeUser`. */
["AttendeeUsersOrderBy"]:AttendeeUsersOrderBy;
	/** A condition to be used against `AttendeeUser` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeUserCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `eventId` field. */
	eventId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `userId` field. */
	userId?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `AttendeeExternal` values. */
["AttendeeExternalsConnection"]: AliasType<{
	/** A list of `AttendeeExternal` objects. */
	nodes?:ValueTypes["AttendeeExternal"],
	/** A list of edges which contains the `AttendeeExternal` and cursor to aid in pagination. */
	edges?:ValueTypes["AttendeeExternalsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `AttendeeExternal` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["AttendeeExternal"]: AliasType<{
	id?:boolean,
	eventId?:boolean,
	firstName?:boolean,
	lastName?:boolean,
	email?:boolean,
	phone?:boolean,
	notes?:boolean,
	birthNumber?:boolean,
	guardianName?:boolean,
	managedBy?:boolean,
	confirmedBy?:boolean,
	confirmedAt?:boolean,
	createdAt?:boolean,
	updatedAt?:boolean,
	tenantId?:boolean,
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ValueTypes["User"],
		__typename?: boolean
}>;
	/** A `AttendeeExternal` edge in the connection. */
["AttendeeExternalsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `AttendeeExternal` at the end of the edge. */
	node?:ValueTypes["AttendeeExternal"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `AttendeeExternal`. */
["AttendeeExternalsOrderBy"]:AttendeeExternalsOrderBy;
	/** A condition to be used against `AttendeeExternal` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeExternalCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `eventId` field. */
	eventId?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `managedBy` field. */
	managedBy?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `confirmedBy` field. */
	confirmedBy?:ValueTypes["BigInt"] | null
};
	/** A `AttendeeUser` edge in the connection. */
["AttendeeUsersEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `AttendeeUser` at the end of the edge. */
	node?:ValueTypes["AttendeeUser"],
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
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ValueTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ValueTypes["GalerieFoto"],
		__typename?: boolean
}>;
	["GalerieFoto"]: AliasType<{
	gfId?:boolean,
	gfIdRodic?:boolean,
	gfName?:boolean,
	gfPath?:boolean,
	gfKdo?:boolean,
	gfTimestamp?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	gdId?:boolean,
	gdIdRodic?:boolean,
	gdName?:boolean,
	gdLevel?:boolean,
	gdPath?:boolean,
	gdHidden?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	/** Checks for equality with the object’s `gfKdo` field. */
	gfKdo?:ValueTypes["BigInt"] | null
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
	/** Checks for equality with the object’s `atFotoMain` field. */
	atFotoMain?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `atTimestampAdd` field. */
	atTimestampAdd?:ValueTypes["Datetime"] | null,
	/** Checks for equality with the object’s `tenantId` field. */
	tenantId?:ValueTypes["BigInt"] | null
};
	/** A `Aktuality` edge in the connection. */
["AktualitiesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Aktuality` at the end of the edge. */
	node?:ValueTypes["Aktuality"],
		__typename?: boolean
}>;
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
	dId?:boolean,
	dPath?:boolean,
	dName?:boolean,
	dFilename?:boolean,
	dKategorie?:boolean,
	dKdo?:boolean,
	dTimestamp?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	/** Checks for equality with the object’s `dKategorie` field. */
	dKategorie?:number | null,
	/** Checks for equality with the object’s `dKdo` field. */
	dKdo?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `dTimestamp` field. */
	dTimestamp?:ValueTypes["Datetime"] | null
};
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
	nId?:boolean,
	nTrener?:boolean,
	nPocetHod?:boolean,
	nMaxPocetHod?:boolean,
	nOd?:boolean,
	nDo?:boolean,
	nVisible?:boolean,
	nLock?:boolean,
	nTimestamp?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	freeLessons?:boolean,
	myLessons?:boolean,
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
	niId?:boolean,
	niIdRodic?:boolean,
	niPartner?:boolean,
	niPocetHod?:boolean,
	niLock?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ValueTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ValueTypes["Pary"],
		__typename?: boolean
}>;
	["Pary"]: AliasType<{
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
	id?:boolean,
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ValueTypes["User"],
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
	niPartner?:ValueTypes["BigInt"] | null
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
	riId?:boolean,
	riIdRodic?:boolean,
	riPartner?:boolean,
	riOd?:boolean,
	riDo?:boolean,
	riLock?:boolean,
	id?:boolean,
	tenantId?:boolean,
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ValueTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ValueTypes["Pary"],
		__typename?: boolean
}>;
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:unknown;
	["Rozpi"]: AliasType<{
	rId?:boolean,
	rTrener?:boolean,
	rKde?:boolean,
	rDatum?:boolean,
	rVisible?:boolean,
	rLock?:boolean,
	rTimestamp?:boolean,
	id?:boolean,
	tenantId?:boolean,
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
	riOd?:ValueTypes["Time"] | null
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
	/** Checks for equality with the object’s `nOd` field. */
	nOd?:ValueTypes["Date"] | null
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
	/** Checks for equality with the object’s `pHodnoceni` field. */
	pHodnoceni?:number | null
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
	pnId?:boolean,
	pnNavrhl?:boolean,
	pnPartner?:boolean,
	pnPartnerka?:boolean,
	id?:boolean,
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
	/** Checks for equality with the object’s `rDatum` field. */
	rDatum?:ValueTypes["Date"] | null
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
	ssId?:boolean,
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
	/** Checks for equality with the object’s `ssUser` field. */
	ssUser?:ValueTypes["BigInt"] | null
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
	/** Checks for equality with the object’s `upTimestampAdd` field. */
	upTimestampAdd?:ValueTypes["Datetime"] | null
};
	/** A connection to a list of `Attachment` values. */
["AttachmentsConnection"]: AliasType<{
	/** A list of `Attachment` objects. */
	nodes?:ValueTypes["Attachment"],
	/** A list of edges which contains the `Attachment` and cursor to aid in pagination. */
	edges?:ValueTypes["AttachmentsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Attachment` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Attachment` edge in the connection. */
["AttachmentsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Attachment` at the end of the edge. */
	node?:ValueTypes["Attachment"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Attachment`. */
["AttachmentsOrderBy"]:AttachmentsOrderBy;
	/** A condition to be used against `Attachment` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AttachmentCondition"]: {
	/** Checks for equality with the object’s `objectName` field. */
	objectName?:string | null,
	/** Checks for equality with the object’s `uploadedBy` field. */
	uploadedBy?:ValueTypes["BigInt"] | null
};
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]:AkceItemsOrderBy;
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
	/** A connection to a list of `Event` values. */
["EventsConnection"]: AliasType<{
	/** A list of `Event` objects. */
	nodes?:ValueTypes["Event"],
	/** A list of edges which contains the `Event` and cursor to aid in pagination. */
	edges?:ValueTypes["EventsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Event` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Event` edge in the connection. */
["EventsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Event` at the end of the edge. */
	node?:ValueTypes["Event"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Event`. */
["EventsOrderBy"]:EventsOrderBy;
	/** A condition to be used against `Event` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["EventCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `since` field. */
	since?:ValueTypes["Date"] | null,
	/** Checks for equality with the object’s `isVisible` field. */
	isVisible?:boolean | null
};
	/** A connection to a list of `FormResponse` values. */
["FormResponsesConnection"]: AliasType<{
	/** A list of `FormResponse` objects. */
	nodes?:ValueTypes["FormResponse"],
	/** A list of edges which contains the `FormResponse` and cursor to aid in pagination. */
	edges?:ValueTypes["FormResponsesEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `FormResponse` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	["FormResponse"]: AliasType<{
	id?:boolean,
	type?:boolean,
	data?:boolean,
	url?:boolean,
	createdAt?:boolean,
	updatedAt?:boolean,
	tenantId?:boolean,
		__typename?: boolean
}>;
	/** A `FormResponse` edge in the connection. */
["FormResponsesEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `FormResponse` at the end of the edge. */
	node?:ValueTypes["FormResponse"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `FormResponse`. */
["FormResponsesOrderBy"]:FormResponsesOrderBy;
	/** A condition to be used against `FormResponse` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["FormResponseCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null,
	/** Checks for equality with the object’s `type` field. */
	type?:string | null,
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?:ValueTypes["Datetime"] | null
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
	gdIdRodic?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `Location` values. */
["LocationsConnection"]: AliasType<{
	/** A list of `Location` objects. */
	nodes?:ValueTypes["Location"],
	/** A list of edges which contains the `Location` and cursor to aid in pagination. */
	edges?:ValueTypes["LocationsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Location` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Location` edge in the connection. */
["LocationsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Location` at the end of the edge. */
	node?:ValueTypes["Location"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Location`. */
["LocationsOrderBy"]:LocationsOrderBy;
	/** A condition to be used against `Location` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["LocationCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null
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
	id?:boolean,
	url?:boolean,
	content?:boolean,
	createdAt?:boolean,
	updatedAt?:boolean,
	title?:boolean,
		__typename?: boolean
}>;
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
	url?:string | null
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
	revNumber?:number | null
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
	paName?:string | null
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
	peId?:ValueTypes["BigInt"] | null
};
	/** A connection to a list of `Person` values. */
["PeopleConnection"]: AliasType<{
	/** A list of `Person` objects. */
	nodes?:ValueTypes["Person"],
	/** A list of edges which contains the `Person` and cursor to aid in pagination. */
	edges?:ValueTypes["PeopleEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Person` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Person` edge in the connection. */
["PeopleEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Person` at the end of the edge. */
	node?:ValueTypes["Person"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Person`. */
["PeopleOrderBy"]:PeopleOrderBy;
	/** A condition to be used against `Person` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PersonCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null
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
	/** Checks for equality with the object’s `pcSymbol` field. */
	pcSymbol?:ValueTypes["BigInt"] | null
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
	pgId?:ValueTypes["BigInt"] | null
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
	/** Checks for equality with the object’s `prHash` field. */
	prHash?:string | null
};
	/** A connection to a list of `Tenant` values. */
["TenantsConnection"]: AliasType<{
	/** A list of `Tenant` objects. */
	nodes?:ValueTypes["Tenant"],
	/** A list of edges which contains the `Tenant` and cursor to aid in pagination. */
	edges?:ValueTypes["TenantsEdge"],
	/** Information to aid in pagination. */
	pageInfo?:ValueTypes["PageInfo"],
	/** The count of *all* `Tenant` you could get from the connection. */
	totalCount?:boolean,
		__typename?: boolean
}>;
	/** A `Tenant` edge in the connection. */
["TenantsEdge"]: AliasType<{
	/** A cursor for use in pagination. */
	cursor?:boolean,
	/** The `Tenant` at the end of the edge. */
	node?:ValueTypes["Tenant"],
		__typename?: boolean
}>;
	/** Methods to use when ordering `Tenant`. */
["TenantsOrderBy"]:TenantsOrderBy;
	/** A condition to be used against `Tenant` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["TenantCondition"]: {
	/** Checks for equality with the object’s `id` field. */
	id?:ValueTypes["BigInt"] | null
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
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: AliasType<{
createAkce?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAkceInput"]},ValueTypes["CreateAkcePayload"]],
createAkceItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAkceItemInput"]},ValueTypes["CreateAkceItemPayload"]],
createAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAktualityInput"]},ValueTypes["CreateAktualityPayload"]],
createAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAttachmentInput"]},ValueTypes["CreateAttachmentPayload"]],
createAttendeeExternal?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAttendeeExternalInput"]},ValueTypes["CreateAttendeeExternalPayload"]],
createAttendeeUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateAttendeeUserInput"]},ValueTypes["CreateAttendeeUserPayload"]],
createCohortGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateCohortGroupInput"]},ValueTypes["CreateCohortGroupPayload"]],
createDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateDokumentyInput"]},ValueTypes["CreateDokumentyPayload"]],
createEvent?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateEventInput"]},ValueTypes["CreateEventPayload"]],
createFormResponse?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateFormResponseInput"]},ValueTypes["CreateFormResponsePayload"]],
createGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateGalerieDirInput"]},ValueTypes["CreateGalerieDirPayload"]],
createGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateGalerieFotoInput"]},ValueTypes["CreateGalerieFotoPayload"]],
createLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateLocationInput"]},ValueTypes["CreateLocationPayload"]],
createLocationAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateLocationAttachmentInput"]},ValueTypes["CreateLocationAttachmentPayload"]],
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
createPerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreatePersonInput"]},ValueTypes["CreatePersonPayload"]],
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
createRoom?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRoomInput"]},ValueTypes["CreateRoomPayload"]],
createRoomAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRoomAttachmentInput"]},ValueTypes["CreateRoomAttachmentPayload"]],
createRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRozpiInput"]},ValueTypes["CreateRozpiPayload"]],
createRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateRozpisItemInput"]},ValueTypes["CreateRozpisItemPayload"]],
createSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateSkupinyInput"]},ValueTypes["CreateSkupinyPayload"]],
createTenant?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateTenantInput"]},ValueTypes["CreateTenantPayload"]],
createTenantAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateTenantAttachmentInput"]},ValueTypes["CreateTenantAttachmentPayload"]],
createTenantLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateTenantLocationInput"]},ValueTypes["CreateTenantLocationPayload"]],
createTenantPerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateTenantPersonInput"]},ValueTypes["CreateTenantPersonPayload"]],
createUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUpozorneniInput"]},ValueTypes["CreateUpozorneniPayload"]],
createUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUpozorneniSkupinyInput"]},ValueTypes["CreateUpozorneniSkupinyPayload"]],
createUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateUserInput"]},ValueTypes["CreateUserPayload"]],
updateAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAktualityInput"]},ValueTypes["UpdateAktualityPayload"]],
updateAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAttachmentInput"]},ValueTypes["UpdateAttachmentPayload"]],
updateAttendeeExternal?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAttendeeExternalInput"]},ValueTypes["UpdateAttendeeExternalPayload"]],
updateAttendeeUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAttendeeUserInput"]},ValueTypes["UpdateAttendeeUserPayload"]],
updateAttendeeUserByUserIdAndEventId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateAttendeeUserByUserIdAndEventIdInput"]},ValueTypes["UpdateAttendeeUserPayload"]],
updateCohortGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateCohortGroupInput"]},ValueTypes["UpdateCohortGroupPayload"]],
updateDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateDokumentyInput"]},ValueTypes["UpdateDokumentyPayload"]],
updateEvent?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateEventInput"]},ValueTypes["UpdateEventPayload"]],
updateFormResponse?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateFormResponseInput"]},ValueTypes["UpdateFormResponsePayload"]],
updateGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieDirInput"]},ValueTypes["UpdateGalerieDirPayload"]],
updateGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateGalerieFotoInput"]},ValueTypes["UpdateGalerieFotoPayload"]],
updateLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateLocationInput"]},ValueTypes["UpdateLocationPayload"]],
updateLocationAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateLocationAttachmentInput"]},ValueTypes["UpdateLocationAttachmentPayload"]],
updateNabidka?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaInput"]},ValueTypes["UpdateNabidkaPayload"]],
updateNabidkaItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaItemInput"]},ValueTypes["UpdateNabidkaItemPayload"]],
updateNabidkaItemByNiPartnerAndNiIdRodic?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateNabidkaItemByNiPartnerAndNiIdRodicInput"]},ValueTypes["UpdateNabidkaItemPayload"]],
updatePage?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePageInput"]},ValueTypes["UpdatePagePayload"]],
updatePageByUrl?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePageByUrlInput"]},ValueTypes["UpdatePagePayload"]],
updateParameter?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParameterInput"]},ValueTypes["UpdateParameterPayload"]],
updatePary?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyInput"]},ValueTypes["UpdateParyPayload"]],
updateParyNavrh?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateParyNavrhInput"]},ValueTypes["UpdateParyNavrhPayload"]],
updatePermission?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePermissionInput"]},ValueTypes["UpdatePermissionPayload"]],
updatePerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePersonInput"]},ValueTypes["UpdatePersonPayload"]],
updatePlatbyCategory?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryInput"]},ValueTypes["UpdatePlatbyCategoryPayload"]],
updatePlatbyCategoryGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyCategoryGroupInput"]},ValueTypes["UpdatePlatbyCategoryGroupPayload"]],
updatePlatbyGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupInput"]},ValueTypes["UpdatePlatbyGroupPayload"]],
updatePlatbyGroupSkupina?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyGroupSkupinaInput"]},ValueTypes["UpdatePlatbyGroupSkupinaPayload"]],
updatePlatbyItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyItemInput"]},ValueTypes["UpdatePlatbyItemPayload"]],
updatePlatbyRaw?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdatePlatbyRawInput"]},ValueTypes["UpdatePlatbyRawPayload"]],
updateRoom?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRoomInput"]},ValueTypes["UpdateRoomPayload"]],
updateRoomAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRoomAttachmentInput"]},ValueTypes["UpdateRoomAttachmentPayload"]],
updateRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpiInput"]},ValueTypes["UpdateRozpiPayload"]],
updateRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateRozpisItemInput"]},ValueTypes["UpdateRozpisItemPayload"]],
updateSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateSkupinyInput"]},ValueTypes["UpdateSkupinyPayload"]],
updateTenant?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateTenantInput"]},ValueTypes["UpdateTenantPayload"]],
updateTenantAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateTenantAttachmentInput"]},ValueTypes["UpdateTenantAttachmentPayload"]],
updateTenantLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateTenantLocationInput"]},ValueTypes["UpdateTenantLocationPayload"]],
updateTenantPerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateTenantPersonInput"]},ValueTypes["UpdateTenantPersonPayload"]],
updateUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniInput"]},ValueTypes["UpdateUpozorneniPayload"]],
updateUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUpozorneniSkupinyInput"]},ValueTypes["UpdateUpozorneniSkupinyPayload"]],
updateUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["UpdateUserInput"]},ValueTypes["UpdateUserPayload"]],
deleteAktuality?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAktualityInput"]},ValueTypes["DeleteAktualityPayload"]],
deleteAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAttachmentInput"]},ValueTypes["DeleteAttachmentPayload"]],
deleteAttendeeExternal?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAttendeeExternalInput"]},ValueTypes["DeleteAttendeeExternalPayload"]],
deleteAttendeeUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAttendeeUserInput"]},ValueTypes["DeleteAttendeeUserPayload"]],
deleteAttendeeUserByUserIdAndEventId?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteAttendeeUserByUserIdAndEventIdInput"]},ValueTypes["DeleteAttendeeUserPayload"]],
deleteCohortGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteCohortGroupInput"]},ValueTypes["DeleteCohortGroupPayload"]],
deleteDokumenty?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteDokumentyInput"]},ValueTypes["DeleteDokumentyPayload"]],
deleteEvent?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteEventInput"]},ValueTypes["DeleteEventPayload"]],
deleteFormResponse?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteFormResponseInput"]},ValueTypes["DeleteFormResponsePayload"]],
deleteGalerieDir?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieDirInput"]},ValueTypes["DeleteGalerieDirPayload"]],
deleteGalerieFoto?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteGalerieFotoInput"]},ValueTypes["DeleteGalerieFotoPayload"]],
deleteLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteLocationInput"]},ValueTypes["DeleteLocationPayload"]],
deleteLocationAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteLocationAttachmentInput"]},ValueTypes["DeleteLocationAttachmentPayload"]],
deleteNabidka?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaInput"]},ValueTypes["DeleteNabidkaPayload"]],
deleteNabidkaItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaItemInput"]},ValueTypes["DeleteNabidkaItemPayload"]],
deleteNabidkaItemByNiPartnerAndNiIdRodic?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteNabidkaItemByNiPartnerAndNiIdRodicInput"]},ValueTypes["DeleteNabidkaItemPayload"]],
deleteParameter?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParameterInput"]},ValueTypes["DeleteParameterPayload"]],
deletePary?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyInput"]},ValueTypes["DeleteParyPayload"]],
deleteParyNavrh?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteParyNavrhInput"]},ValueTypes["DeleteParyNavrhPayload"]],
deletePermission?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePermissionInput"]},ValueTypes["DeletePermissionPayload"]],
deletePerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePersonInput"]},ValueTypes["DeletePersonPayload"]],
deletePlatbyCategory?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryInput"]},ValueTypes["DeletePlatbyCategoryPayload"]],
deletePlatbyCategoryGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyCategoryGroupInput"]},ValueTypes["DeletePlatbyCategoryGroupPayload"]],
deletePlatbyGroup?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupInput"]},ValueTypes["DeletePlatbyGroupPayload"]],
deletePlatbyGroupSkupina?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyGroupSkupinaInput"]},ValueTypes["DeletePlatbyGroupSkupinaPayload"]],
deletePlatbyItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyItemInput"]},ValueTypes["DeletePlatbyItemPayload"]],
deletePlatbyRaw?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeletePlatbyRawInput"]},ValueTypes["DeletePlatbyRawPayload"]],
deleteRoom?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRoomInput"]},ValueTypes["DeleteRoomPayload"]],
deleteRoomAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRoomAttachmentInput"]},ValueTypes["DeleteRoomAttachmentPayload"]],
deleteRozpi?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpiInput"]},ValueTypes["DeleteRozpiPayload"]],
deleteRozpisItem?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteRozpisItemInput"]},ValueTypes["DeleteRozpisItemPayload"]],
deleteSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteSkupinyInput"]},ValueTypes["DeleteSkupinyPayload"]],
deleteTenant?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteTenantInput"]},ValueTypes["DeleteTenantPayload"]],
deleteTenantAttachment?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteTenantAttachmentInput"]},ValueTypes["DeleteTenantAttachmentPayload"]],
deleteTenantLocation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteTenantLocationInput"]},ValueTypes["DeleteTenantLocationPayload"]],
deleteTenantPerson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteTenantPersonInput"]},ValueTypes["DeleteTenantPersonPayload"]],
deleteUpozorneni?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniInput"]},ValueTypes["DeleteUpozorneniPayload"]],
deleteUpozorneniSkupiny?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUpozorneniSkupinyInput"]},ValueTypes["DeleteUpozorneniSkupinyPayload"]],
deleteUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["DeleteUserInput"]},ValueTypes["DeleteUserPayload"]],
bookLesson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["BookLessonInput"]},ValueTypes["BookLessonPayload"]],
cancelLesson?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CancelLessonInput"]},ValueTypes["CancelLessonPayload"]],
cancelParticipation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CancelParticipationInput"]},ValueTypes["CancelParticipationPayload"]],
changePassword?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["ChangePasswordInput"]},ValueTypes["ChangePasswordPayload"]],
confirmUser?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["ConfirmUserInput"]},ValueTypes["ConfirmUserPayload"]],
createCouple?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateCoupleInput"]},ValueTypes["CreateCouplePayload"]],
createParticipation?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateParticipationInput"]},ValueTypes["CreateParticipationPayload"]],
createParticipationExternal?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["CreateParticipationExternalInput"]},ValueTypes["CreateParticipationExternalPayload"]],
fixUnpairedCouples?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["FixUnpairedCouplesInput"]},ValueTypes["FixUnpairedCouplesPayload"]],
login?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["LoginInput"]},ValueTypes["LoginPayload"]],
logout?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["LogoutInput"]},ValueTypes["LogoutPayload"]],
prospectFormDancer?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["ProspectFormDancerInput"]},ValueTypes["ProspectFormDancerPayload"]],
reservationSetDesiredLessons?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["ReservationSetDesiredLessonsInput"]},ValueTypes["ReservationSetDesiredLessonsPayload"]],
resetPassword?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["ResetPasswordInput"]},ValueTypes["ResetPasswordPayload"]],
submitForm?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["SubmitFormInput"]},ValueTypes["SubmitFormPayload"]],
verifyFunction?: [{	/** The exclusive input argument for this mutation. An object type, make sure to see documentation for this object’s fields. */
	input:ValueTypes["VerifyFunctionInput"]},ValueTypes["VerifyFunctionPayload"]],
uploadFile?: [{	fileName:string},ValueTypes["UploadFilePayload"]],
downloadFile?: [{	id:number},boolean],
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
	aJmeno?:string | null,
	aKde?:string | null,
	aInfo?:string | null,
	aOd?:ValueTypes["Date"] | null,
	aDo?:ValueTypes["Date"] | null,
	aKapacita?:ValueTypes["BigInt"] | null,
	aDokumenty?:string | null,
	aTimestamp?:ValueTypes["Datetime"] | null,
	aLock?:boolean | null,
	aVisible?:boolean | null,
	summary?:string | null,
	isPublic?:boolean | null,
	enableNotes?:boolean | null
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
	aiIdRodic?:ValueTypes["BigInt"] | null,
	aiUser?:ValueTypes["BigInt"] | null,
	aiRokNarozeni?:number | null,
	notes?:string | null
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
	atKdo?:ValueTypes["BigInt"] | null,
	atKat?:string | null,
	atJmeno:string,
	atText:string,
	atPreview:string,
	atFoto?:ValueTypes["BigInt"] | null,
	atFotoMain?:ValueTypes["BigInt"] | null,
	atTimestamp?:ValueTypes["Datetime"] | null,
	atTimestampAdd?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Attachment` mutation. */
["CreateAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Attachment` that was created by this mutation. */
	attachment?:ValueTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ValueTypes["User"],
attachmentEdge?: [{	/** The method to use when ordering `Attachment`. */
	orderBy?:ValueTypes["AttachmentsOrderBy"][]},ValueTypes["AttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Attachment` mutation. */
["CreateAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Attachment` to be created by this mutation. */
	attachment:ValueTypes["AttachmentInput"]
};
	/** An input for mutations affecting `Attachment` */
["AttachmentInput"]: {
	objectName:string,
	previewObjectName?:string | null,
	uploadedBy?:ValueTypes["BigInt"] | null,
	uploadedAt?:ValueTypes["Datetime"] | null
};
	/** The output of our create `AttendeeExternal` mutation. */
["CreateAttendeeExternalPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeExternal` that was created by this mutation. */
	attendeeExternal?:ValueTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ValueTypes["User"],
attendeeExternalEdge?: [{	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][]},ValueTypes["AttendeeExternalsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `AttendeeExternal` mutation. */
["CreateAttendeeExternalInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `AttendeeExternal` to be created by this mutation. */
	attendeeExternal:ValueTypes["AttendeeExternalInput"]
};
	/** An input for mutations affecting `AttendeeExternal` */
["AttendeeExternalInput"]: {
	eventId:ValueTypes["BigInt"],
	firstName:string,
	lastName:string,
	email:string,
	phone:string,
	notes?:string | null,
	birthNumber?:string | null,
	guardianName?:string | null,
	managedBy?:ValueTypes["BigInt"] | null,
	confirmedBy?:ValueTypes["BigInt"] | null,
	confirmedAt?:ValueTypes["Datetime"] | null,
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `AttendeeUser` mutation. */
["CreateAttendeeUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeUser` that was created by this mutation. */
	attendeeUser?:ValueTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ValueTypes["User"],
attendeeUserEdge?: [{	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][]},ValueTypes["AttendeeUsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `AttendeeUser` mutation. */
["CreateAttendeeUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `AttendeeUser` to be created by this mutation. */
	attendeeUser:ValueTypes["AttendeeUserInput"]
};
	/** An input for mutations affecting `AttendeeUser` */
["AttendeeUserInput"]: {
	id?:ValueTypes["BigInt"] | null,
	eventId:ValueTypes["BigInt"],
	userId:ValueTypes["BigInt"],
	birthYear:number,
	notes?:string | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `CohortGroup` mutation. */
["CreateCohortGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `CohortGroup` that was created by this mutation. */
	cohortGroup?:ValueTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ValueTypes["Tenant"],
cohortGroupEdge?: [{	/** The method to use when ordering `CohortGroup`. */
	orderBy?:ValueTypes["CohortGroupsOrderBy"][]},ValueTypes["CohortGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `CohortGroup` mutation. */
["CreateCohortGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `CohortGroup` to be created by this mutation. */
	cohortGroup:ValueTypes["CohortGroupInput"]
};
	/** An input for mutations affecting `CohortGroup` */
["CohortGroupInput"]: {
	id?:ValueTypes["BigInt"] | null,
	name:string,
	description?:string | null,
	ordering?:number | null,
	isPublic?:boolean | null,
	tenant?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	dTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Event` mutation. */
["CreateEventPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Event` that was created by this mutation. */
	event?:ValueTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
eventEdge?: [{	/** The method to use when ordering `Event`. */
	orderBy?:ValueTypes["EventsOrderBy"][]},ValueTypes["EventsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Event` mutation. */
["CreateEventInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Event` to be created by this mutation. */
	event:ValueTypes["EventInput"]
};
	/** An input for mutations affecting `Event` */
["EventInput"]: {
	id?:ValueTypes["BigInt"] | null,
	name:string,
	locationText:string,
	description:string,
	since:ValueTypes["Date"],
	until:ValueTypes["Date"],
	capacity?:ValueTypes["BigInt"] | null,
	filesLegacy?:string | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	isLocked?:boolean | null,
	isVisible?:boolean | null,
	summary?:string | null,
	isPublic?:boolean | null,
	enableNotes?:boolean | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `FormResponse` mutation. */
["CreateFormResponsePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `FormResponse` that was created by this mutation. */
	formResponse?:ValueTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
formResponseEdge?: [{	/** The method to use when ordering `FormResponse`. */
	orderBy?:ValueTypes["FormResponsesOrderBy"][]},ValueTypes["FormResponsesEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `FormResponse` mutation. */
["CreateFormResponseInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `FormResponse` to be created by this mutation. */
	formResponse:ValueTypes["FormResponseInput"]
};
	/** An input for mutations affecting `FormResponse` */
["FormResponseInput"]: {
	id?:ValueTypes["BigInt"] | null,
	type:string,
	data:ValueTypes["JSON"],
	url:string,
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	gdHidden?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	gfTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Location` mutation. */
["CreateLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Location` that was created by this mutation. */
	location?:ValueTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
locationEdge?: [{	/** The method to use when ordering `Location`. */
	orderBy?:ValueTypes["LocationsOrderBy"][]},ValueTypes["LocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Location` mutation. */
["CreateLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Location` to be created by this mutation. */
	location:ValueTypes["LocationInput"]
};
	/** An input for mutations affecting `Location` */
["LocationInput"]: {
	id?:ValueTypes["BigInt"] | null,
	name:string,
	description:ValueTypes["JSON"]
};
	/** The output of our create `LocationAttachment` mutation. */
["CreateLocationAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `LocationAttachment` that was created by this mutation. */
	locationAttachment?:ValueTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ValueTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
locationAttachmentEdge?: [{	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][]},ValueTypes["LocationAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `LocationAttachment` mutation. */
["CreateLocationAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `LocationAttachment` to be created by this mutation. */
	locationAttachment:ValueTypes["LocationAttachmentInput"]
};
	/** An input for mutations affecting `LocationAttachment` */
["LocationAttachmentInput"]: {
	locationId:ValueTypes["BigInt"],
	objectName:string
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
	nMaxPocetHod?:number | null,
	nOd:ValueTypes["Date"],
	nDo:ValueTypes["Date"],
	nVisible?:boolean | null,
	nLock?:boolean | null,
	nTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	niLock?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ValueTypes["User"],
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
	pTimestampArchive?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null
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
	pnPartnerka:ValueTypes["BigInt"],
	id?:ValueTypes["BigInt"] | null
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
	peMain:number,
	id?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Person` mutation. */
["CreatePersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Person` that was created by this mutation. */
	person?:ValueTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
personEdge?: [{	/** The method to use when ordering `Person`. */
	orderBy?:ValueTypes["PeopleOrderBy"][]},ValueTypes["PeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Person` mutation. */
["CreatePersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Person` to be created by this mutation. */
	person:ValueTypes["PersonInput"]
};
	/** An input for mutations affecting `Person` */
["PersonInput"]: {
	id?:ValueTypes["BigInt"] | null,
	firstName:string,
	lastName:string,
	gender:ValueTypes["GenderType"]
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
	pcVisible?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	pcgIdCategory:ValueTypes["BigInt"],
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	pgBase?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	pgsIdGroup:ValueTypes["BigInt"],
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	piPrefix?:number | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	prDiscarded?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Room` mutation. */
["CreateRoomPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Room` that was created by this mutation. */
	room?:ValueTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ValueTypes["Location"],
roomEdge?: [{	/** The method to use when ordering `Room`. */
	orderBy?:ValueTypes["RoomsOrderBy"][]},ValueTypes["RoomsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Room` mutation. */
["CreateRoomInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Room` to be created by this mutation. */
	room:ValueTypes["RoomInput"]
};
	/** An input for mutations affecting `Room` */
["RoomInput"]: {
	id?:ValueTypes["BigInt"] | null,
	name:string,
	description:ValueTypes["JSON"],
	location?:ValueTypes["BigInt"] | null
};
	/** The output of our create `RoomAttachment` mutation. */
["CreateRoomAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RoomAttachment` that was created by this mutation. */
	roomAttachment?:ValueTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ValueTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
roomAttachmentEdge?: [{	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][]},ValueTypes["RoomAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `RoomAttachment` mutation. */
["CreateRoomAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `RoomAttachment` to be created by this mutation. */
	roomAttachment:ValueTypes["RoomAttachmentInput"]
};
	/** An input for mutations affecting `RoomAttachment` */
["RoomAttachmentInput"]: {
	roomId:ValueTypes["BigInt"],
	objectName:string
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
	rTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	riLock?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ValueTypes["CohortGroup"],
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
	sColorText?:string | null,
	sLocation?:string | null,
	sVisible?:boolean | null,
	ordering?:number | null,
	internalInfo?:string | null,
	cohortGroup?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our create `Tenant` mutation. */
["CreateTenantPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Tenant` that was created by this mutation. */
	tenant?:ValueTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
tenantEdge?: [{	/** The method to use when ordering `Tenant`. */
	orderBy?:ValueTypes["TenantsOrderBy"][]},ValueTypes["TenantsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `Tenant` mutation. */
["CreateTenantInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `Tenant` to be created by this mutation. */
	tenant:ValueTypes["TenantInput"]
};
	/** An input for mutations affecting `Tenant` */
["TenantInput"]: {
	id?:ValueTypes["BigInt"] | null,
	name:string,
	memberInfo:string,
	origins?:(string | undefined | null)[]
};
	/** The output of our create `TenantAttachment` mutation. */
["CreateTenantAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantAttachment` that was created by this mutation. */
	tenantAttachment?:ValueTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
tenantAttachmentEdge?: [{	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][]},ValueTypes["TenantAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `TenantAttachment` mutation. */
["CreateTenantAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `TenantAttachment` to be created by this mutation. */
	tenantAttachment:ValueTypes["TenantAttachmentInput"]
};
	/** An input for mutations affecting `TenantAttachment` */
["TenantAttachmentInput"]: {
	tenantId:ValueTypes["BigInt"],
	objectName:string,
	type?:ValueTypes["TenantAttachmentType"] | null
};
	/** The output of our create `TenantLocation` mutation. */
["CreateTenantLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantLocation` that was created by this mutation. */
	tenantLocation?:ValueTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ValueTypes["Location"],
tenantLocationEdge?: [{	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][]},ValueTypes["TenantLocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `TenantLocation` mutation. */
["CreateTenantLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `TenantLocation` to be created by this mutation. */
	tenantLocation:ValueTypes["TenantLocationInput"]
};
	/** An input for mutations affecting `TenantLocation` */
["TenantLocationInput"]: {
	tenantId:ValueTypes["BigInt"],
	locationId:ValueTypes["BigInt"]
};
	/** The output of our create `TenantPerson` mutation. */
["CreateTenantPersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantPerson` that was created by this mutation. */
	tenantPerson?:ValueTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ValueTypes["Person"],
tenantPersonEdge?: [{	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][]},ValueTypes["TenantPeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the create `TenantPerson` mutation. */
["CreateTenantPersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** The `TenantPerson` to be created by this mutation. */
	tenantPerson:ValueTypes["TenantPersonInput"]
};
	/** An input for mutations affecting `TenantPerson` */
["TenantPersonInput"]: {
	tenantId:ValueTypes["BigInt"],
	personId:ValueTypes["BigInt"]
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
	upKdo?:ValueTypes["BigInt"] | null,
	upNadpis:string,
	upText:string,
	upBarvy?:ValueTypes["BigInt"] | null,
	upLock?:boolean | null,
	upTimestamp?:ValueTypes["Datetime"] | null,
	upTimestampAdd?:ValueTypes["Datetime"] | null,
	scheduledSince?:ValueTypes["Datetime"] | null,
	scheduledUntil?:ValueTypes["Datetime"] | null,
	isVisible?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	upsPopis:string,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	uGroup?:ValueTypes["BigInt"] | null,
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
	uGdprSignedAt?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch:ValueTypes["AktualityPatch"],
	atId:ValueTypes["BigInt"]
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
	atTimestampAdd?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Attachment` mutation. */
["UpdateAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Attachment` that was updated by this mutation. */
	attachment?:ValueTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ValueTypes["User"],
attachmentEdge?: [{	/** The method to use when ordering `Attachment`. */
	orderBy?:ValueTypes["AttachmentsOrderBy"][]},ValueTypes["AttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAttachment` mutation. */
["UpdateAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Attachment` being updated. */
	patch:ValueTypes["AttachmentPatch"],
	objectName:string
};
	/** Represents an update to a `Attachment`. Fields that are set will be updated. */
["AttachmentPatch"]: {
	objectName?:string | null,
	previewObjectName?:string | null,
	uploadedBy?:ValueTypes["BigInt"] | null,
	uploadedAt?:ValueTypes["Datetime"] | null
};
	/** The output of our update `AttendeeExternal` mutation. */
["UpdateAttendeeExternalPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeExternal` that was updated by this mutation. */
	attendeeExternal?:ValueTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ValueTypes["User"],
attendeeExternalEdge?: [{	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][]},ValueTypes["AttendeeExternalsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAttendeeExternal` mutation. */
["UpdateAttendeeExternalInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `AttendeeExternal` being updated. */
	patch:ValueTypes["AttendeeExternalPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `AttendeeExternal`. Fields that are set will be updated. */
["AttendeeExternalPatch"]: {
	eventId?:ValueTypes["BigInt"] | null,
	firstName?:string | null,
	lastName?:string | null,
	email?:string | null,
	phone?:string | null,
	notes?:string | null,
	birthNumber?:string | null,
	guardianName?:string | null,
	managedBy?:ValueTypes["BigInt"] | null,
	confirmedBy?:ValueTypes["BigInt"] | null,
	confirmedAt?:ValueTypes["Datetime"] | null,
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `AttendeeUser` mutation. */
["UpdateAttendeeUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeUser` that was updated by this mutation. */
	attendeeUser?:ValueTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ValueTypes["User"],
attendeeUserEdge?: [{	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][]},ValueTypes["AttendeeUsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateAttendeeUser` mutation. */
["UpdateAttendeeUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `AttendeeUser` being updated. */
	patch:ValueTypes["AttendeeUserPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `AttendeeUser`. Fields that are set will be updated. */
["AttendeeUserPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	eventId?:ValueTypes["BigInt"] | null,
	userId?:ValueTypes["BigInt"] | null,
	birthYear?:number | null,
	notes?:string | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** All input for the `updateAttendeeUserByUserIdAndEventId` mutation. */
["UpdateAttendeeUserByUserIdAndEventIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `AttendeeUser` being updated. */
	patch:ValueTypes["AttendeeUserPatch"],
	userId:ValueTypes["BigInt"],
	eventId:ValueTypes["BigInt"]
};
	/** The output of our update `CohortGroup` mutation. */
["UpdateCohortGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `CohortGroup` that was updated by this mutation. */
	cohortGroup?:ValueTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ValueTypes["Tenant"],
cohortGroupEdge?: [{	/** The method to use when ordering `CohortGroup`. */
	orderBy?:ValueTypes["CohortGroupsOrderBy"][]},ValueTypes["CohortGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateCohortGroup` mutation. */
["UpdateCohortGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `CohortGroup` being updated. */
	patch:ValueTypes["CohortGroupPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `CohortGroup`. Fields that are set will be updated. */
["CohortGroupPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	name?:string | null,
	description?:string | null,
	ordering?:number | null,
	isPublic?:boolean | null,
	tenant?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch:ValueTypes["DokumentyPatch"],
	dId:ValueTypes["BigInt"]
};
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: {
	dId?:ValueTypes["BigInt"] | null,
	dPath?:string | null,
	dName?:string | null,
	dFilename?:string | null,
	dKategorie?:number | null,
	dKdo?:ValueTypes["BigInt"] | null,
	dTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Event` mutation. */
["UpdateEventPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Event` that was updated by this mutation. */
	event?:ValueTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
eventEdge?: [{	/** The method to use when ordering `Event`. */
	orderBy?:ValueTypes["EventsOrderBy"][]},ValueTypes["EventsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateEvent` mutation. */
["UpdateEventInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Event` being updated. */
	patch:ValueTypes["EventPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `Event`. Fields that are set will be updated. */
["EventPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	name?:string | null,
	locationText?:string | null,
	description?:string | null,
	since?:ValueTypes["Date"] | null,
	until?:ValueTypes["Date"] | null,
	capacity?:ValueTypes["BigInt"] | null,
	filesLegacy?:string | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	isLocked?:boolean | null,
	isVisible?:boolean | null,
	summary?:string | null,
	isPublic?:boolean | null,
	enableNotes?:boolean | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `FormResponse` mutation. */
["UpdateFormResponsePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `FormResponse` that was updated by this mutation. */
	formResponse?:ValueTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
formResponseEdge?: [{	/** The method to use when ordering `FormResponse`. */
	orderBy?:ValueTypes["FormResponsesOrderBy"][]},ValueTypes["FormResponsesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateFormResponse` mutation. */
["UpdateFormResponseInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `FormResponse` being updated. */
	patch:ValueTypes["FormResponsePatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `FormResponse`. Fields that are set will be updated. */
["FormResponsePatch"]: {
	id?:ValueTypes["BigInt"] | null,
	type?:string | null,
	data?:ValueTypes["JSON"] | null,
	url?:string | null,
	createdAt?:ValueTypes["Datetime"] | null,
	updatedAt?:ValueTypes["Datetime"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch:ValueTypes["GalerieDirPatch"],
	gdId:ValueTypes["BigInt"]
};
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: {
	gdId?:ValueTypes["BigInt"] | null,
	gdIdRodic?:ValueTypes["BigInt"] | null,
	gdName?:string | null,
	gdLevel?:number | null,
	gdPath?:string | null,
	gdHidden?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch:ValueTypes["GalerieFotoPatch"],
	gfId:ValueTypes["BigInt"]
};
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: {
	gfId?:ValueTypes["BigInt"] | null,
	gfIdRodic?:ValueTypes["BigInt"] | null,
	gfName?:string | null,
	gfPath?:string | null,
	gfKdo?:ValueTypes["BigInt"] | null,
	gfTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Location` mutation. */
["UpdateLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Location` that was updated by this mutation. */
	location?:ValueTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
locationEdge?: [{	/** The method to use when ordering `Location`. */
	orderBy?:ValueTypes["LocationsOrderBy"][]},ValueTypes["LocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateLocation` mutation. */
["UpdateLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Location` being updated. */
	patch:ValueTypes["LocationPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `Location`. Fields that are set will be updated. */
["LocationPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	name?:string | null,
	description?:ValueTypes["JSON"] | null
};
	/** The output of our update `LocationAttachment` mutation. */
["UpdateLocationAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `LocationAttachment` that was updated by this mutation. */
	locationAttachment?:ValueTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ValueTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
locationAttachmentEdge?: [{	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][]},ValueTypes["LocationAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateLocationAttachment` mutation. */
["UpdateLocationAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `LocationAttachment` being updated. */
	patch:ValueTypes["LocationAttachmentPatch"],
	locationId:ValueTypes["BigInt"],
	objectName:string
};
	/** Represents an update to a `LocationAttachment`. Fields that are set will be updated. */
["LocationAttachmentPatch"]: {
	locationId?:ValueTypes["BigInt"] | null,
	objectName?:string | null
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
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch:ValueTypes["NabidkaPatch"],
	nId:ValueTypes["BigInt"]
};
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: {
	nId?:ValueTypes["BigInt"] | null,
	nTrener?:ValueTypes["BigInt"] | null,
	nPocetHod?:number | null,
	nMaxPocetHod?:number | null,
	nOd?:ValueTypes["Date"] | null,
	nDo?:ValueTypes["Date"] | null,
	nVisible?:boolean | null,
	nLock?:boolean | null,
	nTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch:ValueTypes["NabidkaItemPatch"],
	niId:ValueTypes["BigInt"]
};
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: {
	niId?:ValueTypes["BigInt"] | null,
	niIdRodic?:ValueTypes["BigInt"] | null,
	niPartner?:ValueTypes["BigInt"] | null,
	niPocetHod?:number | null,
	niLock?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** All input for the `updateNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["UpdateNabidkaItemByNiPartnerAndNiIdRodicInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch:ValueTypes["NabidkaItemPatch"],
	niPartner:ValueTypes["BigInt"],
	niIdRodic:ValueTypes["BigInt"]
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
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch:ValueTypes["PagePatch"],
	id:number
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
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch:ValueTypes["ParameterPatch"],
	paName:string
};
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: {
	paName?:string | null,
	paValue?:string | null
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ValueTypes["User"],
paryEdge?: [{	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][]},ValueTypes["PariesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePary` mutation. */
["UpdateParyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Pary` being updated. */
	patch:ValueTypes["ParyPatch"],
	pId:ValueTypes["BigInt"]
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
	pTimestampArchive?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch:ValueTypes["ParyNavrhPatch"],
	pnId:ValueTypes["BigInt"]
};
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: {
	pnId?:ValueTypes["BigInt"] | null,
	pnNavrhl?:ValueTypes["BigInt"] | null,
	pnPartner?:ValueTypes["BigInt"] | null,
	pnPartnerka?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch:ValueTypes["PermissionPatch"],
	peId:ValueTypes["BigInt"]
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
	peMain?:number | null,
	id?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Person` mutation. */
["UpdatePersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Person` that was updated by this mutation. */
	person?:ValueTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
personEdge?: [{	/** The method to use when ordering `Person`. */
	orderBy?:ValueTypes["PeopleOrderBy"][]},ValueTypes["PeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updatePerson` mutation. */
["UpdatePersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Person` being updated. */
	patch:ValueTypes["PersonPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `Person`. Fields that are set will be updated. */
["PersonPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	firstName?:string | null,
	lastName?:string | null,
	gender?:ValueTypes["GenderType"] | null
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
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch:ValueTypes["PlatbyCategoryPatch"],
	pcId:ValueTypes["BigInt"]
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
	pcVisible?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch:ValueTypes["PlatbyCategoryGroupPatch"],
	pcgId:ValueTypes["BigInt"]
};
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: {
	pcgId?:ValueTypes["BigInt"] | null,
	pcgIdGroup?:ValueTypes["BigInt"] | null,
	pcgIdCategory?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch:ValueTypes["PlatbyGroupPatch"],
	pgId:ValueTypes["BigInt"]
};
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: {
	pgId?:ValueTypes["BigInt"] | null,
	pgType?:ValueTypes["BigFloat"] | null,
	pgName?:string | null,
	pgDescription?:string | null,
	pgBase?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch:ValueTypes["PlatbyGroupSkupinaPatch"],
	pgsId:ValueTypes["BigInt"]
};
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: {
	pgsId?:ValueTypes["BigInt"] | null,
	pgsIdSkupina?:ValueTypes["BigInt"] | null,
	pgsIdGroup?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch:ValueTypes["PlatbyItemPatch"],
	piId:ValueTypes["BigInt"]
};
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: {
	piId?:ValueTypes["BigInt"] | null,
	piIdUser?:ValueTypes["BigInt"] | null,
	piIdCategory?:ValueTypes["BigInt"] | null,
	piIdRaw?:ValueTypes["BigInt"] | null,
	piAmount?:ValueTypes["BigFloat"] | null,
	piDate?:ValueTypes["Date"] | null,
	piPrefix?:number | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch:ValueTypes["PlatbyRawPatch"],
	prId:ValueTypes["BigInt"]
};
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: {
	prId?:ValueTypes["BigInt"] | null,
	prRaw?:string | null,
	prHash?:string | null,
	prSorted?:boolean | null,
	prDiscarded?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Room` mutation. */
["UpdateRoomPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Room` that was updated by this mutation. */
	room?:ValueTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ValueTypes["Location"],
roomEdge?: [{	/** The method to use when ordering `Room`. */
	orderBy?:ValueTypes["RoomsOrderBy"][]},ValueTypes["RoomsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateRoom` mutation. */
["UpdateRoomInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Room` being updated. */
	patch:ValueTypes["RoomPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `Room`. Fields that are set will be updated. */
["RoomPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	name?:string | null,
	description?:ValueTypes["JSON"] | null,
	location?:ValueTypes["BigInt"] | null
};
	/** The output of our update `RoomAttachment` mutation. */
["UpdateRoomAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RoomAttachment` that was updated by this mutation. */
	roomAttachment?:ValueTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ValueTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
roomAttachmentEdge?: [{	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][]},ValueTypes["RoomAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateRoomAttachment` mutation. */
["UpdateRoomAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `RoomAttachment` being updated. */
	patch:ValueTypes["RoomAttachmentPatch"],
	roomId:ValueTypes["BigInt"],
	objectName:string
};
	/** Represents an update to a `RoomAttachment`. Fields that are set will be updated. */
["RoomAttachmentPatch"]: {
	roomId?:ValueTypes["BigInt"] | null,
	objectName?:string | null
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
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch:ValueTypes["RozpiPatch"],
	rId:ValueTypes["BigInt"]
};
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: {
	rId?:ValueTypes["BigInt"] | null,
	rTrener?:ValueTypes["BigInt"] | null,
	rKde?:string | null,
	rDatum?:ValueTypes["Date"] | null,
	rVisible?:boolean | null,
	rLock?:boolean | null,
	rTimestamp?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch:ValueTypes["RozpisItemPatch"],
	riId:ValueTypes["BigInt"]
};
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: {
	riId?:ValueTypes["BigInt"] | null,
	riIdRodic?:ValueTypes["BigInt"] | null,
	riPartner?:ValueTypes["BigInt"] | null,
	riOd?:ValueTypes["Time"] | null,
	riDo?:ValueTypes["Time"] | null,
	riLock?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ValueTypes["CohortGroup"],
skupinyEdge?: [{	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][]},ValueTypes["SkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateSkupiny` mutation. */
["UpdateSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Skupiny` being updated. */
	patch:ValueTypes["SkupinyPatch"],
	sId:ValueTypes["BigInt"]
};
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: {
	sId?:ValueTypes["BigInt"] | null,
	sName?:string | null,
	sDescription?:string | null,
	sColorRgb?:string | null,
	sColorText?:string | null,
	sLocation?:string | null,
	sVisible?:boolean | null,
	ordering?:number | null,
	internalInfo?:string | null,
	cohortGroup?:ValueTypes["BigInt"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `Tenant` mutation. */
["UpdateTenantPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Tenant` that was updated by this mutation. */
	tenant?:ValueTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
tenantEdge?: [{	/** The method to use when ordering `Tenant`. */
	orderBy?:ValueTypes["TenantsOrderBy"][]},ValueTypes["TenantsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateTenant` mutation. */
["UpdateTenantInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Tenant` being updated. */
	patch:ValueTypes["TenantPatch"],
	id:ValueTypes["BigInt"]
};
	/** Represents an update to a `Tenant`. Fields that are set will be updated. */
["TenantPatch"]: {
	id?:ValueTypes["BigInt"] | null,
	name?:string | null,
	memberInfo?:string | null,
	origins?:(string | undefined | null)[]
};
	/** The output of our update `TenantAttachment` mutation. */
["UpdateTenantAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantAttachment` that was updated by this mutation. */
	tenantAttachment?:ValueTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
tenantAttachmentEdge?: [{	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][]},ValueTypes["TenantAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateTenantAttachment` mutation. */
["UpdateTenantAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `TenantAttachment` being updated. */
	patch:ValueTypes["TenantAttachmentPatch"],
	tenantId:ValueTypes["BigInt"],
	objectName:string
};
	/** Represents an update to a `TenantAttachment`. Fields that are set will be updated. */
["TenantAttachmentPatch"]: {
	tenantId?:ValueTypes["BigInt"] | null,
	objectName?:string | null,
	type?:ValueTypes["TenantAttachmentType"] | null
};
	/** The output of our update `TenantLocation` mutation. */
["UpdateTenantLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantLocation` that was updated by this mutation. */
	tenantLocation?:ValueTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ValueTypes["Location"],
tenantLocationEdge?: [{	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][]},ValueTypes["TenantLocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateTenantLocation` mutation. */
["UpdateTenantLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `TenantLocation` being updated. */
	patch:ValueTypes["TenantLocationPatch"],
	tenantId:ValueTypes["BigInt"],
	locationId:ValueTypes["BigInt"]
};
	/** Represents an update to a `TenantLocation`. Fields that are set will be updated. */
["TenantLocationPatch"]: {
	tenantId?:ValueTypes["BigInt"] | null,
	locationId?:ValueTypes["BigInt"] | null
};
	/** The output of our update `TenantPerson` mutation. */
["UpdateTenantPersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantPerson` that was updated by this mutation. */
	tenantPerson?:ValueTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ValueTypes["Person"],
tenantPersonEdge?: [{	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][]},ValueTypes["TenantPeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the `updateTenantPerson` mutation. */
["UpdateTenantPersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `TenantPerson` being updated. */
	patch:ValueTypes["TenantPersonPatch"],
	tenantId:ValueTypes["BigInt"],
	personId:ValueTypes["BigInt"]
};
	/** Represents an update to a `TenantPerson`. Fields that are set will be updated. */
["TenantPersonPatch"]: {
	tenantId?:ValueTypes["BigInt"] | null,
	personId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch:ValueTypes["UpozorneniPatch"],
	upId:ValueTypes["BigInt"]
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
	upTimestampAdd?:ValueTypes["Datetime"] | null,
	scheduledSince?:ValueTypes["Datetime"] | null,
	scheduledUntil?:ValueTypes["Datetime"] | null,
	isVisible?:boolean | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch:ValueTypes["UpozorneniSkupinyPatch"],
	upsId:ValueTypes["BigInt"]
};
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: {
	upsId?:ValueTypes["BigInt"] | null,
	upsIdRodic?:ValueTypes["BigInt"] | null,
	upsIdSkupina?:ValueTypes["BigInt"] | null,
	upsColor?:string | null,
	upsPopis?:string | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch:ValueTypes["UserPatch"],
	uId:ValueTypes["BigInt"]
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
	uGdprSignedAt?:ValueTypes["Datetime"] | null,
	id?:ValueTypes["BigInt"] | null,
	tenantId?:ValueTypes["BigInt"] | null
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
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	atId:ValueTypes["BigInt"]
};
	/** The output of our delete `Attachment` mutation. */
["DeleteAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Attachment` that was deleted by this mutation. */
	attachment?:ValueTypes["Attachment"],
	deletedAttachmentNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ValueTypes["User"],
attachmentEdge?: [{	/** The method to use when ordering `Attachment`. */
	orderBy?:ValueTypes["AttachmentsOrderBy"][]},ValueTypes["AttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAttachment` mutation. */
["DeleteAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	objectName:string
};
	/** The output of our delete `AttendeeExternal` mutation. */
["DeleteAttendeeExternalPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeExternal` that was deleted by this mutation. */
	attendeeExternal?:ValueTypes["AttendeeExternal"],
	deletedAttendeeExternalNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ValueTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ValueTypes["User"],
attendeeExternalEdge?: [{	/** The method to use when ordering `AttendeeExternal`. */
	orderBy?:ValueTypes["AttendeeExternalsOrderBy"][]},ValueTypes["AttendeeExternalsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAttendeeExternal` mutation. */
["DeleteAttendeeExternalInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** The output of our delete `AttendeeUser` mutation. */
["DeleteAttendeeUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `AttendeeUser` that was deleted by this mutation. */
	attendeeUser?:ValueTypes["AttendeeUser"],
	deletedAttendeeUserNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ValueTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ValueTypes["User"],
attendeeUserEdge?: [{	/** The method to use when ordering `AttendeeUser`. */
	orderBy?:ValueTypes["AttendeeUsersOrderBy"][]},ValueTypes["AttendeeUsersEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteAttendeeUser` mutation. */
["DeleteAttendeeUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** All input for the `deleteAttendeeUserByUserIdAndEventId` mutation. */
["DeleteAttendeeUserByUserIdAndEventIdInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	userId:ValueTypes["BigInt"],
	eventId:ValueTypes["BigInt"]
};
	/** The output of our delete `CohortGroup` mutation. */
["DeleteCohortGroupPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `CohortGroup` that was deleted by this mutation. */
	cohortGroup?:ValueTypes["CohortGroup"],
	deletedCohortGroupNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ValueTypes["Tenant"],
cohortGroupEdge?: [{	/** The method to use when ordering `CohortGroup`. */
	orderBy?:ValueTypes["CohortGroupsOrderBy"][]},ValueTypes["CohortGroupsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteCohortGroup` mutation. */
["DeleteCohortGroupInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
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
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	dId:ValueTypes["BigInt"]
};
	/** The output of our delete `Event` mutation. */
["DeleteEventPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Event` that was deleted by this mutation. */
	event?:ValueTypes["Event"],
	deletedEventNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
eventEdge?: [{	/** The method to use when ordering `Event`. */
	orderBy?:ValueTypes["EventsOrderBy"][]},ValueTypes["EventsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteEvent` mutation. */
["DeleteEventInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** The output of our delete `FormResponse` mutation. */
["DeleteFormResponsePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `FormResponse` that was deleted by this mutation. */
	formResponse?:ValueTypes["FormResponse"],
	deletedFormResponseNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
formResponseEdge?: [{	/** The method to use when ordering `FormResponse`. */
	orderBy?:ValueTypes["FormResponsesOrderBy"][]},ValueTypes["FormResponsesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteFormResponse` mutation. */
["DeleteFormResponseInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
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
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	gfId:ValueTypes["BigInt"]
};
	/** The output of our delete `Location` mutation. */
["DeleteLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Location` that was deleted by this mutation. */
	location?:ValueTypes["Location"],
	deletedLocationNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
locationEdge?: [{	/** The method to use when ordering `Location`. */
	orderBy?:ValueTypes["LocationsOrderBy"][]},ValueTypes["LocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteLocation` mutation. */
["DeleteLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** The output of our delete `LocationAttachment` mutation. */
["DeleteLocationAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `LocationAttachment` that was deleted by this mutation. */
	locationAttachment?:ValueTypes["LocationAttachment"],
	deletedLocationAttachmentNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ValueTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
locationAttachmentEdge?: [{	/** The method to use when ordering `LocationAttachment`. */
	orderBy?:ValueTypes["LocationAttachmentsOrderBy"][]},ValueTypes["LocationAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteLocationAttachment` mutation. */
["DeleteLocationAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	locationId:ValueTypes["BigInt"],
	objectName:string
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
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	niId:ValueTypes["BigInt"]
};
	/** All input for the `deleteNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["DeleteNabidkaItemByNiPartnerAndNiIdRodicInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	niPartner:ValueTypes["BigInt"],
	niIdRodic:ValueTypes["BigInt"]
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ValueTypes["User"],
paryEdge?: [{	/** The method to use when ordering `Pary`. */
	orderBy?:ValueTypes["PariesOrderBy"][]},ValueTypes["PariesEdge"]],
		__typename?: boolean
}>;
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
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	peId:ValueTypes["BigInt"]
};
	/** The output of our delete `Person` mutation. */
["DeletePersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Person` that was deleted by this mutation. */
	person?:ValueTypes["Person"],
	deletedPersonNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
personEdge?: [{	/** The method to use when ordering `Person`. */
	orderBy?:ValueTypes["PeopleOrderBy"][]},ValueTypes["PeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deletePerson` mutation. */
["DeletePersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
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
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	prId:ValueTypes["BigInt"]
};
	/** The output of our delete `Room` mutation. */
["DeleteRoomPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Room` that was deleted by this mutation. */
	room?:ValueTypes["Room"],
	deletedRoomNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ValueTypes["Location"],
roomEdge?: [{	/** The method to use when ordering `Room`. */
	orderBy?:ValueTypes["RoomsOrderBy"][]},ValueTypes["RoomsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteRoom` mutation. */
["DeleteRoomInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** The output of our delete `RoomAttachment` mutation. */
["DeleteRoomAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `RoomAttachment` that was deleted by this mutation. */
	roomAttachment?:ValueTypes["RoomAttachment"],
	deletedRoomAttachmentNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ValueTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
roomAttachmentEdge?: [{	/** The method to use when ordering `RoomAttachment`. */
	orderBy?:ValueTypes["RoomAttachmentsOrderBy"][]},ValueTypes["RoomAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteRoomAttachment` mutation. */
["DeleteRoomAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	roomId:ValueTypes["BigInt"],
	objectName:string
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
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	riId:ValueTypes["BigInt"]
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ValueTypes["CohortGroup"],
skupinyEdge?: [{	/** The method to use when ordering `Skupiny`. */
	orderBy?:ValueTypes["SkupiniesOrderBy"][]},ValueTypes["SkupiniesEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	sId:ValueTypes["BigInt"]
};
	/** The output of our delete `Tenant` mutation. */
["DeleteTenantPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `Tenant` that was deleted by this mutation. */
	tenant?:ValueTypes["Tenant"],
	deletedTenantNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
tenantEdge?: [{	/** The method to use when ordering `Tenant`. */
	orderBy?:ValueTypes["TenantsOrderBy"][]},ValueTypes["TenantsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteTenant` mutation. */
["DeleteTenantInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"]
};
	/** The output of our delete `TenantAttachment` mutation. */
["DeleteTenantAttachmentPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantAttachment` that was deleted by this mutation. */
	tenantAttachment?:ValueTypes["TenantAttachment"],
	deletedTenantAttachmentNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ValueTypes["Attachment"],
tenantAttachmentEdge?: [{	/** The method to use when ordering `TenantAttachment`. */
	orderBy?:ValueTypes["TenantAttachmentsOrderBy"][]},ValueTypes["TenantAttachmentsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteTenantAttachment` mutation. */
["DeleteTenantAttachmentInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	tenantId:ValueTypes["BigInt"],
	objectName:string
};
	/** The output of our delete `TenantLocation` mutation. */
["DeleteTenantLocationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantLocation` that was deleted by this mutation. */
	tenantLocation?:ValueTypes["TenantLocation"],
	deletedTenantLocationNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ValueTypes["Location"],
tenantLocationEdge?: [{	/** The method to use when ordering `TenantLocation`. */
	orderBy?:ValueTypes["TenantLocationsOrderBy"][]},ValueTypes["TenantLocationsEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteTenantLocation` mutation. */
["DeleteTenantLocationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	tenantId:ValueTypes["BigInt"],
	locationId:ValueTypes["BigInt"]
};
	/** The output of our delete `TenantPerson` mutation. */
["DeleteTenantPersonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** The `TenantPerson` that was deleted by this mutation. */
	tenantPerson?:ValueTypes["TenantPerson"],
	deletedTenantPersonNodeId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ValueTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ValueTypes["Person"],
tenantPersonEdge?: [{	/** The method to use when ordering `TenantPerson`. */
	orderBy?:ValueTypes["TenantPeopleOrderBy"][]},ValueTypes["TenantPeopleEdge"]],
		__typename?: boolean
}>;
	/** All input for the `deleteTenantPerson` mutation. */
["DeleteTenantPersonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	tenantId:ValueTypes["BigInt"],
	personId:ValueTypes["BigInt"]
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
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	uId:ValueTypes["BigInt"]
};
	/** The output of our `bookLesson` mutation. */
["BookLessonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	rozpisItems?:ValueTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `bookLesson` mutation. */
["BookLessonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	lessonId:ValueTypes["BigInt"]
};
	/** The output of our `cancelLesson` mutation. */
["CancelLessonPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	rozpisItems?:ValueTypes["RozpisItem"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `cancelLesson` mutation. */
["CancelLessonInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	lessonId:ValueTypes["BigInt"]
};
	/** The output of our `cancelParticipation` mutation. */
["CancelParticipationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `cancelParticipation` mutation. */
["CancelParticipationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	eventId:ValueTypes["BigInt"]
};
	/** The output of our `changePassword` mutation. */
["ChangePasswordPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `changePassword` mutation. */
["ChangePasswordInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	oldPass:string,
	newPass:string
};
	/** The output of our `confirmUser` mutation. */
["ConfirmUserPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `confirmUser` mutation. */
["ConfirmUserInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	id:ValueTypes["BigInt"],
	grp:ValueTypes["BigInt"],
	cohort:ValueTypes["BigInt"]
};
	/** The output of our `createCouple` mutation. */
["CreateCouplePayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	paries?:ValueTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `createCouple` mutation. */
["CreateCoupleInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	man:ValueTypes["BigInt"],
	woman:ValueTypes["BigInt"]
};
	/** The output of our `createParticipation` mutation. */
["CreateParticipationPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `createParticipation` mutation. */
["CreateParticipationInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	eventId:ValueTypes["BigInt"],
	yearOfBirth:number,
	myNotes:string
};
	/** The output of our `createParticipationExternal` mutation. */
["CreateParticipationExternalPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `createParticipationExternal` mutation. */
["CreateParticipationExternalInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	eventId:ValueTypes["BigInt"],
	firstName:string,
	lastName:string,
	guardianName:string,
	email:string,
	phone:string,
	notes:string,
	birthNumber:string
};
	/** The output of our `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	paries?:ValueTypes["Pary"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null
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
	/** The output of our `prospectFormDancer` mutation. */
["ProspectFormDancerPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `prospectFormDancer` mutation. */
["ProspectFormDancerInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	cohort:ValueTypes["CrmCohort"],
	prospectData:ValueTypes["ProspectDatumInput"],
	origin:string,
	note:string
};
	["CrmCohort"]:CrmCohort;
	/** An input for mutations affecting `ProspectDatum` */
["ProspectDatumInput"]: {
	name?:string | null,
	surname?:string | null,
	email?:string | null,
	phone?:string | null,
	yearofbirth?:string | null
};
	/** The output of our `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	reservation?:ValueTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ValueTypes["User"],
nabidkaEdge?: [{	/** The method to use when ordering `Nabidka`. */
	orderBy?:ValueTypes["NabidkasOrderBy"][]},ValueTypes["NabidkasEdge"]],
		__typename?: boolean
}>;
	/** All input for the `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	reservationId:ValueTypes["BigInt"],
	lessonCount:number
};
	/** The output of our `resetPassword` mutation. */
["ResetPasswordPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `resetPassword` mutation. */
["ResetPasswordInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	login:string,
	email:string
};
	/** The output of our `submitForm` mutation. */
["SubmitFormPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `submitForm` mutation. */
["SubmitFormInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	type:string,
	data:ValueTypes["JSON"],
	url:string
};
	/** The output of our `verifyFunction` mutation. */
["VerifyFunctionPayload"]: AliasType<{
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:boolean,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ValueTypes["Query"],
		__typename?: boolean
}>;
	/** All input for the `verifyFunction` mutation. */
["VerifyFunctionInput"]: {
	/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?:string | null,
	f?:ValueTypes["RegProc"] | null,
	relid?:ValueTypes["RegClass"] | null
};
	/** A builtin object identifier type for a function name */
["RegProc"]:unknown;
	/** A builtin object identifier type for a relation name */
["RegClass"]:unknown;
	["UploadFilePayload"]: AliasType<{
	uploadUrl?:boolean,
	objectName?:boolean,
		__typename?: boolean
}>
  }

export type ModelTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: {
		/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query:ModelTypes["Query"],
	/** Reads and enables pagination through a set of `Akce`. */
	akces?:ModelTypes["AkcesConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItems?:ModelTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualities?:ModelTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Attachment`. */
	attachments?:ModelTypes["AttachmentsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternals?:ModelTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers?:ModelTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `CohortGroup`. */
	cohortGroups?:ModelTypes["CohortGroupsConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumenties?:ModelTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `Event`. */
	events?:ModelTypes["EventsConnection"],
	/** Reads and enables pagination through a set of `FormResponse`. */
	formResponses?:ModelTypes["FormResponsesConnection"],
	/** Reads and enables pagination through a set of `GalerieDir`. */
	galerieDirs?:ModelTypes["GalerieDirsConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotos?:ModelTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `Location`. */
	locations?:ModelTypes["LocationsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachments?:ModelTypes["LocationAttachmentsConnection"],
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
	/** Reads and enables pagination through a set of `Person`. */
	people?:ModelTypes["PeopleConnection"],
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
	/** Reads and enables pagination through a set of `Room`. */
	rooms?:ModelTypes["RoomsConnection"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachments?:ModelTypes["RoomAttachmentsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpis?:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItems?:ModelTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessions?:ModelTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupinies?:ModelTypes["SkupiniesConnection"],
	/** Reads and enables pagination through a set of `Tenant`. */
	tenants?:ModelTypes["TenantsConnection"],
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachments?:ModelTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations?:ModelTypes["TenantLocationsConnection"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople?:ModelTypes["TenantPeopleConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenis?:ModelTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupinies?:ModelTypes["UpozorneniSkupiniesConnection"],
	/** Reads and enables pagination through a set of `User`. */
	users?:ModelTypes["UsersConnection"],
	aktuality?:ModelTypes["Aktuality"],
	attachment?:ModelTypes["Attachment"],
	attendeeExternal?:ModelTypes["AttendeeExternal"],
	attendeeUser?:ModelTypes["AttendeeUser"],
	attendeeUserByUserIdAndEventId?:ModelTypes["AttendeeUser"],
	cohortGroup?:ModelTypes["CohortGroup"],
	dokumenty?:ModelTypes["Dokumenty"],
	event?:ModelTypes["Event"],
	formResponse?:ModelTypes["FormResponse"],
	galerieDir?:ModelTypes["GalerieDir"],
	galerieFoto?:ModelTypes["GalerieFoto"],
	location?:ModelTypes["Location"],
	locationAttachment?:ModelTypes["LocationAttachment"],
	nabidka?:ModelTypes["Nabidka"],
	nabidkaItem?:ModelTypes["NabidkaItem"],
	nabidkaItemByNiPartnerAndNiIdRodic?:ModelTypes["NabidkaItem"],
	page?:ModelTypes["Page"],
	pageByUrl?:ModelTypes["Page"],
	pageRevision?:ModelTypes["PageRevision"],
	parameter?:ModelTypes["Parameter"],
	pary?:ModelTypes["Pary"],
	paryNavrh?:ModelTypes["ParyNavrh"],
	permission?:ModelTypes["Permission"],
	person?:ModelTypes["Person"],
	platbyCategory?:ModelTypes["PlatbyCategory"],
	platbyCategoryGroup?:ModelTypes["PlatbyCategoryGroup"],
	platbyGroup?:ModelTypes["PlatbyGroup"],
	platbyGroupSkupina?:ModelTypes["PlatbyGroupSkupina"],
	platbyItem?:ModelTypes["PlatbyItem"],
	platbyRaw?:ModelTypes["PlatbyRaw"],
	room?:ModelTypes["Room"],
	roomAttachment?:ModelTypes["RoomAttachment"],
	rozpi?:ModelTypes["Rozpi"],
	rozpisItem?:ModelTypes["RozpisItem"],
	session?:ModelTypes["Session"],
	skupiny?:ModelTypes["Skupiny"],
	tenant?:ModelTypes["Tenant"],
	tenantAttachment?:ModelTypes["TenantAttachment"],
	tenantLocation?:ModelTypes["TenantLocation"],
	tenantPerson?:ModelTypes["TenantPerson"],
	upozorneni?:ModelTypes["Upozorneni"],
	upozorneniSkupiny?:ModelTypes["UpozorneniSkupiny"],
	user?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `Pary`. */
	activeCouples?:ModelTypes["PariesConnection"],
	currentCoupleIds?:ModelTypes["CurrentCoupleIdsConnection"],
	/** Reads and enables pagination through a set of `Permission`. */
	currentPermissions?:ModelTypes["PermissionsConnection"],
	currentSessionId?:string,
	currentTenantId?:ModelTypes["BigInt"],
	currentUserId?:ModelTypes["BigInt"],
	getCurrentCouple?:ModelTypes["Pary"],
	getCurrentTenant?:ModelTypes["Tenant"],
	getCurrentUser?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	myAnnouncements?:ModelTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	myLessons?:ModelTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	reservationsForRange?:ModelTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	schedulesForRange?:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	titleVideos?:ModelTypes["VideosConnection"],
	/** Reads and enables pagination through a set of `User`. */
	trainers?:ModelTypes["UsersConnection"]
};
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
		aId?:ModelTypes["BigInt"],
	aJmeno?:string,
	aKde?:string,
	aInfo?:string,
	aOd?:ModelTypes["Date"],
	aDo?:ModelTypes["Date"],
	aKapacita?:ModelTypes["BigInt"],
	aDokumenty?:string,
	aTimestamp?:ModelTypes["Datetime"],
	aLock?:boolean,
	aVisible?:boolean,
	summary?:string,
	isPublic?:boolean,
	enableNotes?:boolean,
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
		aiId?:ModelTypes["BigInt"],
	aiIdRodic?:ModelTypes["BigInt"],
	aiUser?:ModelTypes["BigInt"],
	aiRokNarozeni?:number,
	notes?:string,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?:ModelTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?:ModelTypes["User"]
};
	["User"]: {
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
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?:ModelTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?:ModelTypes["Skupiny"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers:ModelTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtKdo:ModelTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumentiesByDKdo:ModelTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfKdo:ModelTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdUser:ModelTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkasByNTrener:ModelTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartner:ModelTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnNavrhl:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartner:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartnerka:ModelTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpisByRTrener:ModelTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessionsBySsUser:ModelTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenisByUpKdo:ModelTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `Attachment`. */
	attachmentsByUploadedBy:ModelTypes["AttachmentsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternalsByManagedBy:ModelTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternalsByConfirmedBy:ModelTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiUser:ModelTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartnerka:ModelTypes["PariesConnection"],
	dateOfNewestPayment?:ModelTypes["Date"],
	dateOfOldestPayment?:ModelTypes["Date"],
	fullName?:string,
	hasValidPayment?:boolean,
	inPublicCohort?:boolean
};
	["Permission"]: {
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
	id?:ModelTypes["BigInt"],
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
		sId:ModelTypes["BigInt"],
	sName:string,
	sDescription:string,
	sColorRgb:string,
	sColorText:string,
	sLocation:string,
	sVisible:boolean,
	ordering:number,
	internalInfo:string,
	cohortGroup?:ModelTypes["BigInt"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ModelTypes["CohortGroup"],
	/** Reads and enables pagination through a set of `User`. */
	usersByUSkupina:ModelTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdSkupina:ModelTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdSkupina:ModelTypes["UpozorneniSkupiniesConnection"]
};
	["CohortGroup"]: {
		id:ModelTypes["BigInt"],
	name:string,
	description:string,
	ordering:number,
	isPublic:boolean,
	tenant?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ModelTypes["Tenant"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupiniesByCohortGroup:ModelTypes["SkupiniesConnection"]
};
	["Tenant"]: {
		id:ModelTypes["BigInt"],
	name:string,
	memberInfo:string,
	origins?:string[],
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachments:ModelTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople:ModelTypes["TenantPeopleConnection"],
	/** Reads and enables pagination through a set of `CohortGroup`. */
	cohortGroupsByTenant:ModelTypes["CohortGroupsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations:ModelTypes["TenantLocationsConnection"]
};
	/** A connection to a list of `TenantAttachment` values. */
["TenantAttachmentsConnection"]: {
		/** A list of `TenantAttachment` objects. */
	nodes:ModelTypes["TenantAttachment"][],
	/** A list of edges which contains the `TenantAttachment` and cursor to aid in pagination. */
	edges:ModelTypes["TenantAttachmentsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `TenantAttachment` you could get from the connection. */
	totalCount:number
};
	["TenantAttachment"]: {
		tenantId:ModelTypes["BigInt"],
	objectName:string,
	type?:ModelTypes["TenantAttachmentType"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"]
};
	["TenantAttachmentType"]: GraphQLTypes["TenantAttachmentType"];
	["Attachment"]: {
		objectName:string,
	previewObjectName?:string,
	uploadedBy?:ModelTypes["BigInt"],
	uploadedAt:ModelTypes["Datetime"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachmentsByObjectName:ModelTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachmentsByObjectName:ModelTypes["LocationAttachmentsConnection"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachmentsByObjectName:ModelTypes["RoomAttachmentsConnection"]
};
	/** Methods to use when ordering `TenantAttachment`. */
["TenantAttachmentsOrderBy"]: GraphQLTypes["TenantAttachmentsOrderBy"];
	/** A condition to be used against `TenantAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantAttachmentCondition"]: GraphQLTypes["TenantAttachmentCondition"];
	/** A connection to a list of `LocationAttachment` values. */
["LocationAttachmentsConnection"]: {
		/** A list of `LocationAttachment` objects. */
	nodes:ModelTypes["LocationAttachment"][],
	/** A list of edges which contains the `LocationAttachment` and cursor to aid in pagination. */
	edges:ModelTypes["LocationAttachmentsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `LocationAttachment` you could get from the connection. */
	totalCount:number
};
	["LocationAttachment"]: {
		locationId:ModelTypes["BigInt"],
	objectName:string,
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ModelTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"]
};
	["Location"]: {
		id:ModelTypes["BigInt"],
	name:string,
	description:ModelTypes["JSON"],
	/** Reads and enables pagination through a set of `Room`. */
	roomsByLocation:ModelTypes["RoomsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachments:ModelTypes["LocationAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations:ModelTypes["TenantLocationsConnection"]
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	/** A connection to a list of `Room` values. */
["RoomsConnection"]: {
		/** A list of `Room` objects. */
	nodes:ModelTypes["Room"][],
	/** A list of edges which contains the `Room` and cursor to aid in pagination. */
	edges:ModelTypes["RoomsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Room` you could get from the connection. */
	totalCount:number
};
	["Room"]: {
		id:ModelTypes["BigInt"],
	name:string,
	description:ModelTypes["JSON"],
	location?:ModelTypes["BigInt"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ModelTypes["Location"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachments:ModelTypes["RoomAttachmentsConnection"]
};
	/** A connection to a list of `RoomAttachment` values. */
["RoomAttachmentsConnection"]: {
		/** A list of `RoomAttachment` objects. */
	nodes:ModelTypes["RoomAttachment"][],
	/** A list of edges which contains the `RoomAttachment` and cursor to aid in pagination. */
	edges:ModelTypes["RoomAttachmentsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `RoomAttachment` you could get from the connection. */
	totalCount:number
};
	["RoomAttachment"]: {
		roomId:ModelTypes["BigInt"],
	objectName:string,
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ModelTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"]
};
	/** A `RoomAttachment` edge in the connection. */
["RoomAttachmentsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `RoomAttachment` at the end of the edge. */
	node:ModelTypes["RoomAttachment"]
};
	/** Methods to use when ordering `RoomAttachment`. */
["RoomAttachmentsOrderBy"]: GraphQLTypes["RoomAttachmentsOrderBy"];
	/** A condition to be used against `RoomAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["RoomAttachmentCondition"]: GraphQLTypes["RoomAttachmentCondition"];
	/** A `Room` edge in the connection. */
["RoomsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Room` at the end of the edge. */
	node:ModelTypes["Room"]
};
	/** Methods to use when ordering `Room`. */
["RoomsOrderBy"]: GraphQLTypes["RoomsOrderBy"];
	/** A condition to be used against `Room` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RoomCondition"]: GraphQLTypes["RoomCondition"];
	/** Methods to use when ordering `LocationAttachment`. */
["LocationAttachmentsOrderBy"]: GraphQLTypes["LocationAttachmentsOrderBy"];
	/** A condition to be used against `LocationAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["LocationAttachmentCondition"]: GraphQLTypes["LocationAttachmentCondition"];
	/** A connection to a list of `TenantLocation` values. */
["TenantLocationsConnection"]: {
		/** A list of `TenantLocation` objects. */
	nodes:ModelTypes["TenantLocation"][],
	/** A list of edges which contains the `TenantLocation` and cursor to aid in pagination. */
	edges:ModelTypes["TenantLocationsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `TenantLocation` you could get from the connection. */
	totalCount:number
};
	["TenantLocation"]: {
		tenantId:ModelTypes["BigInt"],
	locationId:ModelTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ModelTypes["Location"]
};
	/** A `TenantLocation` edge in the connection. */
["TenantLocationsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `TenantLocation` at the end of the edge. */
	node:ModelTypes["TenantLocation"]
};
	/** Methods to use when ordering `TenantLocation`. */
["TenantLocationsOrderBy"]: GraphQLTypes["TenantLocationsOrderBy"];
	/** A condition to be used against `TenantLocation` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantLocationCondition"]: GraphQLTypes["TenantLocationCondition"];
	/** A `LocationAttachment` edge in the connection. */
["LocationAttachmentsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `LocationAttachment` at the end of the edge. */
	node:ModelTypes["LocationAttachment"]
};
	/** A `TenantAttachment` edge in the connection. */
["TenantAttachmentsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `TenantAttachment` at the end of the edge. */
	node:ModelTypes["TenantAttachment"]
};
	/** A connection to a list of `TenantPerson` values. */
["TenantPeopleConnection"]: {
		/** A list of `TenantPerson` objects. */
	nodes:ModelTypes["TenantPerson"][],
	/** A list of edges which contains the `TenantPerson` and cursor to aid in pagination. */
	edges:ModelTypes["TenantPeopleEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `TenantPerson` you could get from the connection. */
	totalCount:number
};
	["TenantPerson"]: {
		tenantId:ModelTypes["BigInt"],
	personId:ModelTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ModelTypes["Person"]
};
	["Person"]: {
		id:ModelTypes["BigInt"],
	firstName:string,
	lastName:string,
	gender:ModelTypes["GenderType"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople:ModelTypes["TenantPeopleConnection"]
};
	["GenderType"]: GraphQLTypes["GenderType"];
	/** Methods to use when ordering `TenantPerson`. */
["TenantPeopleOrderBy"]: GraphQLTypes["TenantPeopleOrderBy"];
	/** A condition to be used against `TenantPerson` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantPersonCondition"]: GraphQLTypes["TenantPersonCondition"];
	/** A `TenantPerson` edge in the connection. */
["TenantPeopleEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `TenantPerson` at the end of the edge. */
	node:ModelTypes["TenantPerson"]
};
	/** A connection to a list of `CohortGroup` values. */
["CohortGroupsConnection"]: {
		/** A list of `CohortGroup` objects. */
	nodes:ModelTypes["CohortGroup"][],
	/** A list of edges which contains the `CohortGroup` and cursor to aid in pagination. */
	edges:ModelTypes["CohortGroupsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `CohortGroup` you could get from the connection. */
	totalCount:number
};
	/** A `CohortGroup` edge in the connection. */
["CohortGroupsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `CohortGroup` at the end of the edge. */
	node:ModelTypes["CohortGroup"]
};
	/** Methods to use when ordering `CohortGroup`. */
["CohortGroupsOrderBy"]: GraphQLTypes["CohortGroupsOrderBy"];
	/** A condition to be used against `CohortGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["CohortGroupCondition"]: GraphQLTypes["CohortGroupCondition"];
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
		pgsId:ModelTypes["BigInt"],
	pgsIdSkupina:ModelTypes["BigInt"],
	pgsIdGroup:ModelTypes["BigInt"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?:ModelTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?:ModelTypes["PlatbyGroup"]
};
	["PlatbyGroup"]: {
		pgId:ModelTypes["BigInt"],
	pgType:ModelTypes["BigFloat"],
	pgName:string,
	pgDescription:string,
	pgBase:ModelTypes["BigInt"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		pcgId:ModelTypes["BigInt"],
	pcgIdGroup:ModelTypes["BigInt"],
	pcgIdCategory:ModelTypes["BigInt"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?:ModelTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?:ModelTypes["PlatbyCategory"]
};
	["PlatbyCategory"]: {
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
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		piId:ModelTypes["BigInt"],
	piIdUser?:ModelTypes["BigInt"],
	piIdCategory:ModelTypes["BigInt"],
	piIdRaw?:ModelTypes["BigInt"],
	piAmount:ModelTypes["BigFloat"],
	piDate:ModelTypes["Date"],
	piPrefix:number,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?:ModelTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?:ModelTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?:ModelTypes["PlatbyRaw"]
};
	["PlatbyRaw"]: {
		prId:ModelTypes["BigInt"],
	prRaw:string,
	prHash:string,
	prSorted:boolean,
	prDiscarded:boolean,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		upsId:ModelTypes["BigInt"],
	upsIdRodic:ModelTypes["BigInt"],
	upsIdSkupina:ModelTypes["BigInt"],
	upsColor:string,
	upsPopis:string,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?:ModelTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?:ModelTypes["Skupiny"]
};
	["Upozorneni"]: {
		upId:ModelTypes["BigInt"],
	upKdo?:ModelTypes["BigInt"],
	upNadpis:string,
	upText:string,
	upBarvy:ModelTypes["BigInt"],
	upLock:boolean,
	upTimestamp?:ModelTypes["Datetime"],
	upTimestampAdd:ModelTypes["Datetime"],
	scheduledSince?:ModelTypes["Datetime"],
	scheduledUntil?:ModelTypes["Datetime"],
	isVisible?:boolean,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
	/** A connection to a list of `AttendeeUser` values. */
["AttendeeUsersConnection"]: {
		/** A list of `AttendeeUser` objects. */
	nodes:ModelTypes["AttendeeUser"][],
	/** A list of edges which contains the `AttendeeUser` and cursor to aid in pagination. */
	edges:ModelTypes["AttendeeUsersEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `AttendeeUser` you could get from the connection. */
	totalCount:number
};
	["AttendeeUser"]: {
		id:ModelTypes["BigInt"],
	eventId:ModelTypes["BigInt"],
	userId:ModelTypes["BigInt"],
	birthYear:number,
	notes:string,
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ModelTypes["User"]
};
	["Event"]: {
		id:ModelTypes["BigInt"],
	name:string,
	locationText:string,
	description:string,
	since:ModelTypes["Date"],
	until:ModelTypes["Date"],
	capacity:ModelTypes["BigInt"],
	filesLegacy:string,
	updatedAt?:ModelTypes["Datetime"],
	isLocked:boolean,
	isVisible:boolean,
	summary:string,
	isPublic:boolean,
	enableNotes:boolean,
	tenantId:ModelTypes["BigInt"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers:ModelTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternals:ModelTypes["AttendeeExternalsConnection"],
	remainingSpots?:number
};
	/** Methods to use when ordering `AttendeeUser`. */
["AttendeeUsersOrderBy"]: GraphQLTypes["AttendeeUsersOrderBy"];
	/** A condition to be used against `AttendeeUser` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeUserCondition"]: GraphQLTypes["AttendeeUserCondition"];
	/** A connection to a list of `AttendeeExternal` values. */
["AttendeeExternalsConnection"]: {
		/** A list of `AttendeeExternal` objects. */
	nodes:ModelTypes["AttendeeExternal"][],
	/** A list of edges which contains the `AttendeeExternal` and cursor to aid in pagination. */
	edges:ModelTypes["AttendeeExternalsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `AttendeeExternal` you could get from the connection. */
	totalCount:number
};
	["AttendeeExternal"]: {
		id:ModelTypes["BigInt"],
	eventId:ModelTypes["BigInt"],
	firstName:string,
	lastName:string,
	email:string,
	phone:string,
	notes:string,
	birthNumber?:string,
	guardianName:string,
	managedBy?:ModelTypes["BigInt"],
	confirmedBy?:ModelTypes["BigInt"],
	confirmedAt?:ModelTypes["Datetime"],
	createdAt:ModelTypes["Datetime"],
	updatedAt:ModelTypes["Datetime"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ModelTypes["User"]
};
	/** A `AttendeeExternal` edge in the connection. */
["AttendeeExternalsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `AttendeeExternal` at the end of the edge. */
	node:ModelTypes["AttendeeExternal"]
};
	/** Methods to use when ordering `AttendeeExternal`. */
["AttendeeExternalsOrderBy"]: GraphQLTypes["AttendeeExternalsOrderBy"];
	/** A condition to be used against `AttendeeExternal` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeExternalCondition"]: GraphQLTypes["AttendeeExternalCondition"];
	/** A `AttendeeUser` edge in the connection. */
["AttendeeUsersEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `AttendeeUser` at the end of the edge. */
	node:ModelTypes["AttendeeUser"]
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
		atId:ModelTypes["BigInt"],
	atKdo?:ModelTypes["BigInt"],
	atKat:string,
	atJmeno:string,
	atText:string,
	atPreview:string,
	atFoto?:ModelTypes["BigInt"],
	atFotoMain?:ModelTypes["BigInt"],
	atTimestamp?:ModelTypes["Datetime"],
	atTimestampAdd?:ModelTypes["Datetime"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?:ModelTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?:ModelTypes["GalerieFoto"]
};
	["GalerieFoto"]: {
		gfId:ModelTypes["BigInt"],
	gfIdRodic:ModelTypes["BigInt"],
	gfName:string,
	gfPath:string,
	gfKdo:ModelTypes["BigInt"],
	gfTimestamp?:ModelTypes["Datetime"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?:ModelTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtFotoMain:ModelTypes["AktualitiesConnection"]
};
	["GalerieDir"]: {
		gdId:ModelTypes["BigInt"],
	gdIdRodic:ModelTypes["BigInt"],
	gdName:string,
	gdLevel:number,
	gdPath:string,
	gdHidden:boolean,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		dId:ModelTypes["BigInt"],
	dPath:string,
	dName:string,
	dFilename:string,
	dKategorie:number,
	dKdo:ModelTypes["BigInt"],
	dTimestamp?:ModelTypes["Datetime"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		nId:ModelTypes["BigInt"],
	nTrener:ModelTypes["BigInt"],
	nPocetHod:number,
	nMaxPocetHod:number,
	nOd:ModelTypes["Date"],
	nDo:ModelTypes["Date"],
	nVisible:boolean,
	nLock:boolean,
	nTimestamp?:ModelTypes["Datetime"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiIdRodic:ModelTypes["NabidkaItemsConnection"],
	freeLessons?:number,
	myLessons?:number
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
		niId:ModelTypes["BigInt"],
	niIdRodic:ModelTypes["BigInt"],
	niPartner:ModelTypes["BigInt"],
	niPocetHod:number,
	niLock:boolean,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?:ModelTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?:ModelTypes["Pary"]
};
	["Pary"]: {
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
	id?:ModelTypes["BigInt"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ModelTypes["User"],
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
		riId:ModelTypes["BigInt"],
	riIdRodic:ModelTypes["BigInt"],
	riPartner?:ModelTypes["BigInt"],
	riOd:ModelTypes["Time"],
	riDo:ModelTypes["Time"],
	riLock:boolean,
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?:ModelTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?:ModelTypes["Pary"]
};
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:any;
	["Rozpi"]: {
		rId:ModelTypes["BigInt"],
	rTrener:ModelTypes["BigInt"],
	rKde:string,
	rDatum:ModelTypes["Date"],
	rVisible:boolean,
	rLock:boolean,
	rTimestamp?:ModelTypes["Datetime"],
	id?:ModelTypes["BigInt"],
	tenantId:ModelTypes["BigInt"],
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
		pnId:ModelTypes["BigInt"],
	pnNavrhl:ModelTypes["BigInt"],
	pnPartner:ModelTypes["BigInt"],
	pnPartnerka:ModelTypes["BigInt"],
	id?:ModelTypes["BigInt"],
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
		ssId:string,
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
	/** A connection to a list of `Attachment` values. */
["AttachmentsConnection"]: {
		/** A list of `Attachment` objects. */
	nodes:ModelTypes["Attachment"][],
	/** A list of edges which contains the `Attachment` and cursor to aid in pagination. */
	edges:ModelTypes["AttachmentsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Attachment` you could get from the connection. */
	totalCount:number
};
	/** A `Attachment` edge in the connection. */
["AttachmentsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Attachment` at the end of the edge. */
	node:ModelTypes["Attachment"]
};
	/** Methods to use when ordering `Attachment`. */
["AttachmentsOrderBy"]: GraphQLTypes["AttachmentsOrderBy"];
	/** A condition to be used against `Attachment` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AttachmentCondition"]: GraphQLTypes["AttachmentCondition"];
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]: GraphQLTypes["AkceItemsOrderBy"];
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
	/** A connection to a list of `Event` values. */
["EventsConnection"]: {
		/** A list of `Event` objects. */
	nodes:ModelTypes["Event"][],
	/** A list of edges which contains the `Event` and cursor to aid in pagination. */
	edges:ModelTypes["EventsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Event` you could get from the connection. */
	totalCount:number
};
	/** A `Event` edge in the connection. */
["EventsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Event` at the end of the edge. */
	node:ModelTypes["Event"]
};
	/** Methods to use when ordering `Event`. */
["EventsOrderBy"]: GraphQLTypes["EventsOrderBy"];
	/** A condition to be used against `Event` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["EventCondition"]: GraphQLTypes["EventCondition"];
	/** A connection to a list of `FormResponse` values. */
["FormResponsesConnection"]: {
		/** A list of `FormResponse` objects. */
	nodes:ModelTypes["FormResponse"][],
	/** A list of edges which contains the `FormResponse` and cursor to aid in pagination. */
	edges:ModelTypes["FormResponsesEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `FormResponse` you could get from the connection. */
	totalCount:number
};
	["FormResponse"]: {
		id:ModelTypes["BigInt"],
	type:string,
	data:ModelTypes["JSON"],
	url:string,
	createdAt:ModelTypes["Datetime"],
	updatedAt:ModelTypes["Datetime"],
	tenantId:ModelTypes["BigInt"]
};
	/** A `FormResponse` edge in the connection. */
["FormResponsesEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `FormResponse` at the end of the edge. */
	node:ModelTypes["FormResponse"]
};
	/** Methods to use when ordering `FormResponse`. */
["FormResponsesOrderBy"]: GraphQLTypes["FormResponsesOrderBy"];
	/** A condition to be used against `FormResponse` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["FormResponseCondition"]: GraphQLTypes["FormResponseCondition"];
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
	/** A connection to a list of `Location` values. */
["LocationsConnection"]: {
		/** A list of `Location` objects. */
	nodes:ModelTypes["Location"][],
	/** A list of edges which contains the `Location` and cursor to aid in pagination. */
	edges:ModelTypes["LocationsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Location` you could get from the connection. */
	totalCount:number
};
	/** A `Location` edge in the connection. */
["LocationsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Location` at the end of the edge. */
	node:ModelTypes["Location"]
};
	/** Methods to use when ordering `Location`. */
["LocationsOrderBy"]: GraphQLTypes["LocationsOrderBy"];
	/** A condition to be used against `Location` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["LocationCondition"]: GraphQLTypes["LocationCondition"];
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
		id:number,
	url:string,
	content:ModelTypes["JSON"],
	createdAt:ModelTypes["Datetime"],
	updatedAt:ModelTypes["Datetime"],
	title:string
};
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
	/** A connection to a list of `Person` values. */
["PeopleConnection"]: {
		/** A list of `Person` objects. */
	nodes:ModelTypes["Person"][],
	/** A list of edges which contains the `Person` and cursor to aid in pagination. */
	edges:ModelTypes["PeopleEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Person` you could get from the connection. */
	totalCount:number
};
	/** A `Person` edge in the connection. */
["PeopleEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Person` at the end of the edge. */
	node:ModelTypes["Person"]
};
	/** Methods to use when ordering `Person`. */
["PeopleOrderBy"]: GraphQLTypes["PeopleOrderBy"];
	/** A condition to be used against `Person` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PersonCondition"]: GraphQLTypes["PersonCondition"];
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
	/** A connection to a list of `Tenant` values. */
["TenantsConnection"]: {
		/** A list of `Tenant` objects. */
	nodes:ModelTypes["Tenant"][],
	/** A list of edges which contains the `Tenant` and cursor to aid in pagination. */
	edges:ModelTypes["TenantsEdge"][],
	/** Information to aid in pagination. */
	pageInfo:ModelTypes["PageInfo"],
	/** The count of *all* `Tenant` you could get from the connection. */
	totalCount:number
};
	/** A `Tenant` edge in the connection. */
["TenantsEdge"]: {
		/** A cursor for use in pagination. */
	cursor?:ModelTypes["Cursor"],
	/** The `Tenant` at the end of the edge. */
	node:ModelTypes["Tenant"]
};
	/** Methods to use when ordering `Tenant`. */
["TenantsOrderBy"]: GraphQLTypes["TenantsOrderBy"];
	/** A condition to be used against `Tenant` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["TenantCondition"]: GraphQLTypes["TenantCondition"];
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
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: {
		/** Creates a single `Akce`. */
	createAkce?:ModelTypes["CreateAkcePayload"],
	/** Creates a single `AkceItem`. */
	createAkceItem?:ModelTypes["CreateAkceItemPayload"],
	/** Creates a single `Aktuality`. */
	createAktuality?:ModelTypes["CreateAktualityPayload"],
	/** Creates a single `Attachment`. */
	createAttachment?:ModelTypes["CreateAttachmentPayload"],
	/** Creates a single `AttendeeExternal`. */
	createAttendeeExternal?:ModelTypes["CreateAttendeeExternalPayload"],
	/** Creates a single `AttendeeUser`. */
	createAttendeeUser?:ModelTypes["CreateAttendeeUserPayload"],
	/** Creates a single `CohortGroup`. */
	createCohortGroup?:ModelTypes["CreateCohortGroupPayload"],
	/** Creates a single `Dokumenty`. */
	createDokumenty?:ModelTypes["CreateDokumentyPayload"],
	/** Creates a single `Event`. */
	createEvent?:ModelTypes["CreateEventPayload"],
	/** Creates a single `FormResponse`. */
	createFormResponse?:ModelTypes["CreateFormResponsePayload"],
	/** Creates a single `GalerieDir`. */
	createGalerieDir?:ModelTypes["CreateGalerieDirPayload"],
	/** Creates a single `GalerieFoto`. */
	createGalerieFoto?:ModelTypes["CreateGalerieFotoPayload"],
	/** Creates a single `Location`. */
	createLocation?:ModelTypes["CreateLocationPayload"],
	/** Creates a single `LocationAttachment`. */
	createLocationAttachment?:ModelTypes["CreateLocationAttachmentPayload"],
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
	/** Creates a single `Person`. */
	createPerson?:ModelTypes["CreatePersonPayload"],
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
	/** Creates a single `Room`. */
	createRoom?:ModelTypes["CreateRoomPayload"],
	/** Creates a single `RoomAttachment`. */
	createRoomAttachment?:ModelTypes["CreateRoomAttachmentPayload"],
	/** Creates a single `Rozpi`. */
	createRozpi?:ModelTypes["CreateRozpiPayload"],
	/** Creates a single `RozpisItem`. */
	createRozpisItem?:ModelTypes["CreateRozpisItemPayload"],
	/** Creates a single `Skupiny`. */
	createSkupiny?:ModelTypes["CreateSkupinyPayload"],
	/** Creates a single `Tenant`. */
	createTenant?:ModelTypes["CreateTenantPayload"],
	/** Creates a single `TenantAttachment`. */
	createTenantAttachment?:ModelTypes["CreateTenantAttachmentPayload"],
	/** Creates a single `TenantLocation`. */
	createTenantLocation?:ModelTypes["CreateTenantLocationPayload"],
	/** Creates a single `TenantPerson`. */
	createTenantPerson?:ModelTypes["CreateTenantPersonPayload"],
	/** Creates a single `Upozorneni`. */
	createUpozorneni?:ModelTypes["CreateUpozorneniPayload"],
	/** Creates a single `UpozorneniSkupiny`. */
	createUpozorneniSkupiny?:ModelTypes["CreateUpozorneniSkupinyPayload"],
	/** Creates a single `User`. */
	createUser?:ModelTypes["CreateUserPayload"],
	/** Updates a single `Aktuality` using a unique key and a patch. */
	updateAktuality?:ModelTypes["UpdateAktualityPayload"],
	/** Updates a single `Attachment` using a unique key and a patch. */
	updateAttachment?:ModelTypes["UpdateAttachmentPayload"],
	/** Updates a single `AttendeeExternal` using a unique key and a patch. */
	updateAttendeeExternal?:ModelTypes["UpdateAttendeeExternalPayload"],
	/** Updates a single `AttendeeUser` using a unique key and a patch. */
	updateAttendeeUser?:ModelTypes["UpdateAttendeeUserPayload"],
	/** Updates a single `AttendeeUser` using a unique key and a patch. */
	updateAttendeeUserByUserIdAndEventId?:ModelTypes["UpdateAttendeeUserPayload"],
	/** Updates a single `CohortGroup` using a unique key and a patch. */
	updateCohortGroup?:ModelTypes["UpdateCohortGroupPayload"],
	/** Updates a single `Dokumenty` using a unique key and a patch. */
	updateDokumenty?:ModelTypes["UpdateDokumentyPayload"],
	/** Updates a single `Event` using a unique key and a patch. */
	updateEvent?:ModelTypes["UpdateEventPayload"],
	/** Updates a single `FormResponse` using a unique key and a patch. */
	updateFormResponse?:ModelTypes["UpdateFormResponsePayload"],
	/** Updates a single `GalerieDir` using a unique key and a patch. */
	updateGalerieDir?:ModelTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieFoto` using a unique key and a patch. */
	updateGalerieFoto?:ModelTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `Location` using a unique key and a patch. */
	updateLocation?:ModelTypes["UpdateLocationPayload"],
	/** Updates a single `LocationAttachment` using a unique key and a patch. */
	updateLocationAttachment?:ModelTypes["UpdateLocationAttachmentPayload"],
	/** Updates a single `Nabidka` using a unique key and a patch. */
	updateNabidka?:ModelTypes["UpdateNabidkaPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItem?:ModelTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItemByNiPartnerAndNiIdRodic?:ModelTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePage?:ModelTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePageByUrl?:ModelTypes["UpdatePagePayload"],
	/** Updates a single `Parameter` using a unique key and a patch. */
	updateParameter?:ModelTypes["UpdateParameterPayload"],
	/** Updates a single `Pary` using a unique key and a patch. */
	updatePary?:ModelTypes["UpdateParyPayload"],
	/** Updates a single `ParyNavrh` using a unique key and a patch. */
	updateParyNavrh?:ModelTypes["UpdateParyNavrhPayload"],
	/** Updates a single `Permission` using a unique key and a patch. */
	updatePermission?:ModelTypes["UpdatePermissionPayload"],
	/** Updates a single `Person` using a unique key and a patch. */
	updatePerson?:ModelTypes["UpdatePersonPayload"],
	/** Updates a single `PlatbyCategory` using a unique key and a patch. */
	updatePlatbyCategory?:ModelTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategoryGroup` using a unique key and a patch. */
	updatePlatbyCategoryGroup?:ModelTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyGroup` using a unique key and a patch. */
	updatePlatbyGroup?:ModelTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroupSkupina` using a unique key and a patch. */
	updatePlatbyGroupSkupina?:ModelTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyItem` using a unique key and a patch. */
	updatePlatbyItem?:ModelTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyRaw` using a unique key and a patch. */
	updatePlatbyRaw?:ModelTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `Room` using a unique key and a patch. */
	updateRoom?:ModelTypes["UpdateRoomPayload"],
	/** Updates a single `RoomAttachment` using a unique key and a patch. */
	updateRoomAttachment?:ModelTypes["UpdateRoomAttachmentPayload"],
	/** Updates a single `Rozpi` using a unique key and a patch. */
	updateRozpi?:ModelTypes["UpdateRozpiPayload"],
	/** Updates a single `RozpisItem` using a unique key and a patch. */
	updateRozpisItem?:ModelTypes["UpdateRozpisItemPayload"],
	/** Updates a single `Skupiny` using a unique key and a patch. */
	updateSkupiny?:ModelTypes["UpdateSkupinyPayload"],
	/** Updates a single `Tenant` using a unique key and a patch. */
	updateTenant?:ModelTypes["UpdateTenantPayload"],
	/** Updates a single `TenantAttachment` using a unique key and a patch. */
	updateTenantAttachment?:ModelTypes["UpdateTenantAttachmentPayload"],
	/** Updates a single `TenantLocation` using a unique key and a patch. */
	updateTenantLocation?:ModelTypes["UpdateTenantLocationPayload"],
	/** Updates a single `TenantPerson` using a unique key and a patch. */
	updateTenantPerson?:ModelTypes["UpdateTenantPersonPayload"],
	/** Updates a single `Upozorneni` using a unique key and a patch. */
	updateUpozorneni?:ModelTypes["UpdateUpozorneniPayload"],
	/** Updates a single `UpozorneniSkupiny` using a unique key and a patch. */
	updateUpozorneniSkupiny?:ModelTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `User` using a unique key and a patch. */
	updateUser?:ModelTypes["UpdateUserPayload"],
	/** Deletes a single `Aktuality` using a unique key. */
	deleteAktuality?:ModelTypes["DeleteAktualityPayload"],
	/** Deletes a single `Attachment` using a unique key. */
	deleteAttachment?:ModelTypes["DeleteAttachmentPayload"],
	/** Deletes a single `AttendeeExternal` using a unique key. */
	deleteAttendeeExternal?:ModelTypes["DeleteAttendeeExternalPayload"],
	/** Deletes a single `AttendeeUser` using a unique key. */
	deleteAttendeeUser?:ModelTypes["DeleteAttendeeUserPayload"],
	/** Deletes a single `AttendeeUser` using a unique key. */
	deleteAttendeeUserByUserIdAndEventId?:ModelTypes["DeleteAttendeeUserPayload"],
	/** Deletes a single `CohortGroup` using a unique key. */
	deleteCohortGroup?:ModelTypes["DeleteCohortGroupPayload"],
	/** Deletes a single `Dokumenty` using a unique key. */
	deleteDokumenty?:ModelTypes["DeleteDokumentyPayload"],
	/** Deletes a single `Event` using a unique key. */
	deleteEvent?:ModelTypes["DeleteEventPayload"],
	/** Deletes a single `FormResponse` using a unique key. */
	deleteFormResponse?:ModelTypes["DeleteFormResponsePayload"],
	/** Deletes a single `GalerieDir` using a unique key. */
	deleteGalerieDir?:ModelTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieFoto` using a unique key. */
	deleteGalerieFoto?:ModelTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `Location` using a unique key. */
	deleteLocation?:ModelTypes["DeleteLocationPayload"],
	/** Deletes a single `LocationAttachment` using a unique key. */
	deleteLocationAttachment?:ModelTypes["DeleteLocationAttachmentPayload"],
	/** Deletes a single `Nabidka` using a unique key. */
	deleteNabidka?:ModelTypes["DeleteNabidkaPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItem?:ModelTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItemByNiPartnerAndNiIdRodic?:ModelTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `Parameter` using a unique key. */
	deleteParameter?:ModelTypes["DeleteParameterPayload"],
	/** Deletes a single `Pary` using a unique key. */
	deletePary?:ModelTypes["DeleteParyPayload"],
	/** Deletes a single `ParyNavrh` using a unique key. */
	deleteParyNavrh?:ModelTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `Permission` using a unique key. */
	deletePermission?:ModelTypes["DeletePermissionPayload"],
	/** Deletes a single `Person` using a unique key. */
	deletePerson?:ModelTypes["DeletePersonPayload"],
	/** Deletes a single `PlatbyCategory` using a unique key. */
	deletePlatbyCategory?:ModelTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using a unique key. */
	deletePlatbyCategoryGroup?:ModelTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyGroup` using a unique key. */
	deletePlatbyGroup?:ModelTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using a unique key. */
	deletePlatbyGroupSkupina?:ModelTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyItem` using a unique key. */
	deletePlatbyItem?:ModelTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyRaw` using a unique key. */
	deletePlatbyRaw?:ModelTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `Room` using a unique key. */
	deleteRoom?:ModelTypes["DeleteRoomPayload"],
	/** Deletes a single `RoomAttachment` using a unique key. */
	deleteRoomAttachment?:ModelTypes["DeleteRoomAttachmentPayload"],
	/** Deletes a single `Rozpi` using a unique key. */
	deleteRozpi?:ModelTypes["DeleteRozpiPayload"],
	/** Deletes a single `RozpisItem` using a unique key. */
	deleteRozpisItem?:ModelTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `Skupiny` using a unique key. */
	deleteSkupiny?:ModelTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Tenant` using a unique key. */
	deleteTenant?:ModelTypes["DeleteTenantPayload"],
	/** Deletes a single `TenantAttachment` using a unique key. */
	deleteTenantAttachment?:ModelTypes["DeleteTenantAttachmentPayload"],
	/** Deletes a single `TenantLocation` using a unique key. */
	deleteTenantLocation?:ModelTypes["DeleteTenantLocationPayload"],
	/** Deletes a single `TenantPerson` using a unique key. */
	deleteTenantPerson?:ModelTypes["DeleteTenantPersonPayload"],
	/** Deletes a single `Upozorneni` using a unique key. */
	deleteUpozorneni?:ModelTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `UpozorneniSkupiny` using a unique key. */
	deleteUpozorneniSkupiny?:ModelTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `User` using a unique key. */
	deleteUser?:ModelTypes["DeleteUserPayload"],
	bookLesson?:ModelTypes["BookLessonPayload"],
	cancelLesson?:ModelTypes["CancelLessonPayload"],
	cancelParticipation?:ModelTypes["CancelParticipationPayload"],
	changePassword?:ModelTypes["ChangePasswordPayload"],
	confirmUser?:ModelTypes["ConfirmUserPayload"],
	createCouple?:ModelTypes["CreateCouplePayload"],
	createParticipation?:ModelTypes["CreateParticipationPayload"],
	createParticipationExternal?:ModelTypes["CreateParticipationExternalPayload"],
	fixUnpairedCouples?:ModelTypes["FixUnpairedCouplesPayload"],
	login?:ModelTypes["LoginPayload"],
	logout?:ModelTypes["LogoutPayload"],
	prospectFormDancer?:ModelTypes["ProspectFormDancerPayload"],
	reservationSetDesiredLessons?:ModelTypes["ReservationSetDesiredLessonsPayload"],
	resetPassword?:ModelTypes["ResetPasswordPayload"],
	submitForm?:ModelTypes["SubmitFormPayload"],
	verifyFunction?:ModelTypes["VerifyFunctionPayload"],
	uploadFile:ModelTypes["UploadFilePayload"],
	downloadFile:string
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
	/** The output of our create `Attachment` mutation. */
["CreateAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Attachment` that was created by this mutation. */
	attachment?:ModelTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ModelTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?:ModelTypes["AttachmentsEdge"]
};
	/** All input for the create `Attachment` mutation. */
["CreateAttachmentInput"]: GraphQLTypes["CreateAttachmentInput"];
	/** An input for mutations affecting `Attachment` */
["AttachmentInput"]: GraphQLTypes["AttachmentInput"];
	/** The output of our create `AttendeeExternal` mutation. */
["CreateAttendeeExternalPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeExternal` that was created by this mutation. */
	attendeeExternal?:ModelTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ModelTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?:ModelTypes["AttendeeExternalsEdge"]
};
	/** All input for the create `AttendeeExternal` mutation. */
["CreateAttendeeExternalInput"]: GraphQLTypes["CreateAttendeeExternalInput"];
	/** An input for mutations affecting `AttendeeExternal` */
["AttendeeExternalInput"]: GraphQLTypes["AttendeeExternalInput"];
	/** The output of our create `AttendeeUser` mutation. */
["CreateAttendeeUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeUser` that was created by this mutation. */
	attendeeUser?:ModelTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ModelTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?:ModelTypes["AttendeeUsersEdge"]
};
	/** All input for the create `AttendeeUser` mutation. */
["CreateAttendeeUserInput"]: GraphQLTypes["CreateAttendeeUserInput"];
	/** An input for mutations affecting `AttendeeUser` */
["AttendeeUserInput"]: GraphQLTypes["AttendeeUserInput"];
	/** The output of our create `CohortGroup` mutation. */
["CreateCohortGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `CohortGroup` that was created by this mutation. */
	cohortGroup?:ModelTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ModelTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?:ModelTypes["CohortGroupsEdge"]
};
	/** All input for the create `CohortGroup` mutation. */
["CreateCohortGroupInput"]: GraphQLTypes["CreateCohortGroupInput"];
	/** An input for mutations affecting `CohortGroup` */
["CohortGroupInput"]: GraphQLTypes["CohortGroupInput"];
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
	/** The output of our create `Event` mutation. */
["CreateEventPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Event` that was created by this mutation. */
	event?:ModelTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?:ModelTypes["EventsEdge"]
};
	/** All input for the create `Event` mutation. */
["CreateEventInput"]: GraphQLTypes["CreateEventInput"];
	/** An input for mutations affecting `Event` */
["EventInput"]: GraphQLTypes["EventInput"];
	/** The output of our create `FormResponse` mutation. */
["CreateFormResponsePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `FormResponse` that was created by this mutation. */
	formResponse?:ModelTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?:ModelTypes["FormResponsesEdge"]
};
	/** All input for the create `FormResponse` mutation. */
["CreateFormResponseInput"]: GraphQLTypes["CreateFormResponseInput"];
	/** An input for mutations affecting `FormResponse` */
["FormResponseInput"]: GraphQLTypes["FormResponseInput"];
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
	/** The output of our create `Location` mutation. */
["CreateLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Location` that was created by this mutation. */
	location?:ModelTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?:ModelTypes["LocationsEdge"]
};
	/** All input for the create `Location` mutation. */
["CreateLocationInput"]: GraphQLTypes["CreateLocationInput"];
	/** An input for mutations affecting `Location` */
["LocationInput"]: GraphQLTypes["LocationInput"];
	/** The output of our create `LocationAttachment` mutation. */
["CreateLocationAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `LocationAttachment` that was created by this mutation. */
	locationAttachment?:ModelTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ModelTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?:ModelTypes["LocationAttachmentsEdge"]
};
	/** All input for the create `LocationAttachment` mutation. */
["CreateLocationAttachmentInput"]: GraphQLTypes["CreateLocationAttachmentInput"];
	/** An input for mutations affecting `LocationAttachment` */
["LocationAttachmentInput"]: GraphQLTypes["LocationAttachmentInput"];
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ModelTypes["User"],
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
	/** The output of our create `Person` mutation. */
["CreatePersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Person` that was created by this mutation. */
	person?:ModelTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?:ModelTypes["PeopleEdge"]
};
	/** All input for the create `Person` mutation. */
["CreatePersonInput"]: GraphQLTypes["CreatePersonInput"];
	/** An input for mutations affecting `Person` */
["PersonInput"]: GraphQLTypes["PersonInput"];
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
	/** The output of our create `Room` mutation. */
["CreateRoomPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Room` that was created by this mutation. */
	room?:ModelTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ModelTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?:ModelTypes["RoomsEdge"]
};
	/** All input for the create `Room` mutation. */
["CreateRoomInput"]: GraphQLTypes["CreateRoomInput"];
	/** An input for mutations affecting `Room` */
["RoomInput"]: GraphQLTypes["RoomInput"];
	/** The output of our create `RoomAttachment` mutation. */
["CreateRoomAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RoomAttachment` that was created by this mutation. */
	roomAttachment?:ModelTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ModelTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?:ModelTypes["RoomAttachmentsEdge"]
};
	/** All input for the create `RoomAttachment` mutation. */
["CreateRoomAttachmentInput"]: GraphQLTypes["CreateRoomAttachmentInput"];
	/** An input for mutations affecting `RoomAttachment` */
["RoomAttachmentInput"]: GraphQLTypes["RoomAttachmentInput"];
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
	/** The output of our create `Skupiny` mutation. */
["CreateSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Skupiny` that was created by this mutation. */
	skupiny?:ModelTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ModelTypes["CohortGroup"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the create `Skupiny` mutation. */
["CreateSkupinyInput"]: GraphQLTypes["CreateSkupinyInput"];
	/** An input for mutations affecting `Skupiny` */
["SkupinyInput"]: GraphQLTypes["SkupinyInput"];
	/** The output of our create `Tenant` mutation. */
["CreateTenantPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Tenant` that was created by this mutation. */
	tenant?:ModelTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?:ModelTypes["TenantsEdge"]
};
	/** All input for the create `Tenant` mutation. */
["CreateTenantInput"]: GraphQLTypes["CreateTenantInput"];
	/** An input for mutations affecting `Tenant` */
["TenantInput"]: GraphQLTypes["TenantInput"];
	/** The output of our create `TenantAttachment` mutation. */
["CreateTenantAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantAttachment` that was created by this mutation. */
	tenantAttachment?:ModelTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?:ModelTypes["TenantAttachmentsEdge"]
};
	/** All input for the create `TenantAttachment` mutation. */
["CreateTenantAttachmentInput"]: GraphQLTypes["CreateTenantAttachmentInput"];
	/** An input for mutations affecting `TenantAttachment` */
["TenantAttachmentInput"]: GraphQLTypes["TenantAttachmentInput"];
	/** The output of our create `TenantLocation` mutation. */
["CreateTenantLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantLocation` that was created by this mutation. */
	tenantLocation?:ModelTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ModelTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?:ModelTypes["TenantLocationsEdge"]
};
	/** All input for the create `TenantLocation` mutation. */
["CreateTenantLocationInput"]: GraphQLTypes["CreateTenantLocationInput"];
	/** An input for mutations affecting `TenantLocation` */
["TenantLocationInput"]: GraphQLTypes["TenantLocationInput"];
	/** The output of our create `TenantPerson` mutation. */
["CreateTenantPersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantPerson` that was created by this mutation. */
	tenantPerson?:ModelTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ModelTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?:ModelTypes["TenantPeopleEdge"]
};
	/** All input for the create `TenantPerson` mutation. */
["CreateTenantPersonInput"]: GraphQLTypes["CreateTenantPersonInput"];
	/** An input for mutations affecting `TenantPerson` */
["TenantPersonInput"]: GraphQLTypes["TenantPersonInput"];
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
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: GraphQLTypes["UpdateAktualityInput"];
	/** Represents an update to a `Aktuality`. Fields that are set will be updated. */
["AktualityPatch"]: GraphQLTypes["AktualityPatch"];
	/** The output of our update `Attachment` mutation. */
["UpdateAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Attachment` that was updated by this mutation. */
	attachment?:ModelTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ModelTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?:ModelTypes["AttachmentsEdge"]
};
	/** All input for the `updateAttachment` mutation. */
["UpdateAttachmentInput"]: GraphQLTypes["UpdateAttachmentInput"];
	/** Represents an update to a `Attachment`. Fields that are set will be updated. */
["AttachmentPatch"]: GraphQLTypes["AttachmentPatch"];
	/** The output of our update `AttendeeExternal` mutation. */
["UpdateAttendeeExternalPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeExternal` that was updated by this mutation. */
	attendeeExternal?:ModelTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ModelTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?:ModelTypes["AttendeeExternalsEdge"]
};
	/** All input for the `updateAttendeeExternal` mutation. */
["UpdateAttendeeExternalInput"]: GraphQLTypes["UpdateAttendeeExternalInput"];
	/** Represents an update to a `AttendeeExternal`. Fields that are set will be updated. */
["AttendeeExternalPatch"]: GraphQLTypes["AttendeeExternalPatch"];
	/** The output of our update `AttendeeUser` mutation. */
["UpdateAttendeeUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeUser` that was updated by this mutation. */
	attendeeUser?:ModelTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ModelTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?:ModelTypes["AttendeeUsersEdge"]
};
	/** All input for the `updateAttendeeUser` mutation. */
["UpdateAttendeeUserInput"]: GraphQLTypes["UpdateAttendeeUserInput"];
	/** Represents an update to a `AttendeeUser`. Fields that are set will be updated. */
["AttendeeUserPatch"]: GraphQLTypes["AttendeeUserPatch"];
	/** All input for the `updateAttendeeUserByUserIdAndEventId` mutation. */
["UpdateAttendeeUserByUserIdAndEventIdInput"]: GraphQLTypes["UpdateAttendeeUserByUserIdAndEventIdInput"];
	/** The output of our update `CohortGroup` mutation. */
["UpdateCohortGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `CohortGroup` that was updated by this mutation. */
	cohortGroup?:ModelTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ModelTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?:ModelTypes["CohortGroupsEdge"]
};
	/** All input for the `updateCohortGroup` mutation. */
["UpdateCohortGroupInput"]: GraphQLTypes["UpdateCohortGroupInput"];
	/** Represents an update to a `CohortGroup`. Fields that are set will be updated. */
["CohortGroupPatch"]: GraphQLTypes["CohortGroupPatch"];
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
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: GraphQLTypes["UpdateDokumentyInput"];
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: GraphQLTypes["DokumentyPatch"];
	/** The output of our update `Event` mutation. */
["UpdateEventPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Event` that was updated by this mutation. */
	event?:ModelTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?:ModelTypes["EventsEdge"]
};
	/** All input for the `updateEvent` mutation. */
["UpdateEventInput"]: GraphQLTypes["UpdateEventInput"];
	/** Represents an update to a `Event`. Fields that are set will be updated. */
["EventPatch"]: GraphQLTypes["EventPatch"];
	/** The output of our update `FormResponse` mutation. */
["UpdateFormResponsePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `FormResponse` that was updated by this mutation. */
	formResponse?:ModelTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?:ModelTypes["FormResponsesEdge"]
};
	/** All input for the `updateFormResponse` mutation. */
["UpdateFormResponseInput"]: GraphQLTypes["UpdateFormResponseInput"];
	/** Represents an update to a `FormResponse`. Fields that are set will be updated. */
["FormResponsePatch"]: GraphQLTypes["FormResponsePatch"];
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
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: GraphQLTypes["UpdateGalerieDirInput"];
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: GraphQLTypes["GalerieDirPatch"];
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
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: GraphQLTypes["UpdateGalerieFotoInput"];
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: GraphQLTypes["GalerieFotoPatch"];
	/** The output of our update `Location` mutation. */
["UpdateLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Location` that was updated by this mutation. */
	location?:ModelTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?:ModelTypes["LocationsEdge"]
};
	/** All input for the `updateLocation` mutation. */
["UpdateLocationInput"]: GraphQLTypes["UpdateLocationInput"];
	/** Represents an update to a `Location`. Fields that are set will be updated. */
["LocationPatch"]: GraphQLTypes["LocationPatch"];
	/** The output of our update `LocationAttachment` mutation. */
["UpdateLocationAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `LocationAttachment` that was updated by this mutation. */
	locationAttachment?:ModelTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ModelTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?:ModelTypes["LocationAttachmentsEdge"]
};
	/** All input for the `updateLocationAttachment` mutation. */
["UpdateLocationAttachmentInput"]: GraphQLTypes["UpdateLocationAttachmentInput"];
	/** Represents an update to a `LocationAttachment`. Fields that are set will be updated. */
["LocationAttachmentPatch"]: GraphQLTypes["LocationAttachmentPatch"];
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
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: GraphQLTypes["UpdateNabidkaInput"];
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: GraphQLTypes["NabidkaPatch"];
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
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: GraphQLTypes["UpdateNabidkaItemInput"];
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: GraphQLTypes["NabidkaItemPatch"];
	/** All input for the `updateNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["UpdateNabidkaItemByNiPartnerAndNiIdRodicInput"]: GraphQLTypes["UpdateNabidkaItemByNiPartnerAndNiIdRodicInput"];
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
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: GraphQLTypes["UpdatePageInput"];
	/** Represents an update to a `Page`. Fields that are set will be updated. */
["PagePatch"]: GraphQLTypes["PagePatch"];
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
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: GraphQLTypes["UpdateParameterInput"];
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: GraphQLTypes["ParameterPatch"];
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ModelTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?:ModelTypes["PariesEdge"]
};
	/** All input for the `updatePary` mutation. */
["UpdateParyInput"]: GraphQLTypes["UpdateParyInput"];
	/** Represents an update to a `Pary`. Fields that are set will be updated. */
["ParyPatch"]: GraphQLTypes["ParyPatch"];
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
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: GraphQLTypes["UpdateParyNavrhInput"];
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: GraphQLTypes["ParyNavrhPatch"];
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
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: GraphQLTypes["UpdatePermissionInput"];
	/** Represents an update to a `Permission`. Fields that are set will be updated. */
["PermissionPatch"]: GraphQLTypes["PermissionPatch"];
	/** The output of our update `Person` mutation. */
["UpdatePersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Person` that was updated by this mutation. */
	person?:ModelTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?:ModelTypes["PeopleEdge"]
};
	/** All input for the `updatePerson` mutation. */
["UpdatePersonInput"]: GraphQLTypes["UpdatePersonInput"];
	/** Represents an update to a `Person`. Fields that are set will be updated. */
["PersonPatch"]: GraphQLTypes["PersonPatch"];
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
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: GraphQLTypes["UpdatePlatbyCategoryInput"];
	/** Represents an update to a `PlatbyCategory`. Fields that are set will be updated. */
["PlatbyCategoryPatch"]: GraphQLTypes["PlatbyCategoryPatch"];
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
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: GraphQLTypes["UpdatePlatbyCategoryGroupInput"];
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: GraphQLTypes["PlatbyCategoryGroupPatch"];
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
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: GraphQLTypes["UpdatePlatbyGroupInput"];
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: GraphQLTypes["PlatbyGroupPatch"];
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
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: GraphQLTypes["UpdatePlatbyGroupSkupinaInput"];
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: GraphQLTypes["PlatbyGroupSkupinaPatch"];
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
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: GraphQLTypes["UpdatePlatbyItemInput"];
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: GraphQLTypes["PlatbyItemPatch"];
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
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: GraphQLTypes["UpdatePlatbyRawInput"];
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: GraphQLTypes["PlatbyRawPatch"];
	/** The output of our update `Room` mutation. */
["UpdateRoomPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Room` that was updated by this mutation. */
	room?:ModelTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ModelTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?:ModelTypes["RoomsEdge"]
};
	/** All input for the `updateRoom` mutation. */
["UpdateRoomInput"]: GraphQLTypes["UpdateRoomInput"];
	/** Represents an update to a `Room`. Fields that are set will be updated. */
["RoomPatch"]: GraphQLTypes["RoomPatch"];
	/** The output of our update `RoomAttachment` mutation. */
["UpdateRoomAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RoomAttachment` that was updated by this mutation. */
	roomAttachment?:ModelTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ModelTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?:ModelTypes["RoomAttachmentsEdge"]
};
	/** All input for the `updateRoomAttachment` mutation. */
["UpdateRoomAttachmentInput"]: GraphQLTypes["UpdateRoomAttachmentInput"];
	/** Represents an update to a `RoomAttachment`. Fields that are set will be updated. */
["RoomAttachmentPatch"]: GraphQLTypes["RoomAttachmentPatch"];
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
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: GraphQLTypes["UpdateRozpiInput"];
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: GraphQLTypes["RozpiPatch"];
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
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: GraphQLTypes["UpdateRozpisItemInput"];
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: GraphQLTypes["RozpisItemPatch"];
	/** The output of our update `Skupiny` mutation. */
["UpdateSkupinyPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Skupiny` that was updated by this mutation. */
	skupiny?:ModelTypes["Skupiny"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ModelTypes["CohortGroup"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the `updateSkupiny` mutation. */
["UpdateSkupinyInput"]: GraphQLTypes["UpdateSkupinyInput"];
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: GraphQLTypes["SkupinyPatch"];
	/** The output of our update `Tenant` mutation. */
["UpdateTenantPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Tenant` that was updated by this mutation. */
	tenant?:ModelTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?:ModelTypes["TenantsEdge"]
};
	/** All input for the `updateTenant` mutation. */
["UpdateTenantInput"]: GraphQLTypes["UpdateTenantInput"];
	/** Represents an update to a `Tenant`. Fields that are set will be updated. */
["TenantPatch"]: GraphQLTypes["TenantPatch"];
	/** The output of our update `TenantAttachment` mutation. */
["UpdateTenantAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantAttachment` that was updated by this mutation. */
	tenantAttachment?:ModelTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?:ModelTypes["TenantAttachmentsEdge"]
};
	/** All input for the `updateTenantAttachment` mutation. */
["UpdateTenantAttachmentInput"]: GraphQLTypes["UpdateTenantAttachmentInput"];
	/** Represents an update to a `TenantAttachment`. Fields that are set will be updated. */
["TenantAttachmentPatch"]: GraphQLTypes["TenantAttachmentPatch"];
	/** The output of our update `TenantLocation` mutation. */
["UpdateTenantLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantLocation` that was updated by this mutation. */
	tenantLocation?:ModelTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ModelTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?:ModelTypes["TenantLocationsEdge"]
};
	/** All input for the `updateTenantLocation` mutation. */
["UpdateTenantLocationInput"]: GraphQLTypes["UpdateTenantLocationInput"];
	/** Represents an update to a `TenantLocation`. Fields that are set will be updated. */
["TenantLocationPatch"]: GraphQLTypes["TenantLocationPatch"];
	/** The output of our update `TenantPerson` mutation. */
["UpdateTenantPersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantPerson` that was updated by this mutation. */
	tenantPerson?:ModelTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ModelTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?:ModelTypes["TenantPeopleEdge"]
};
	/** All input for the `updateTenantPerson` mutation. */
["UpdateTenantPersonInput"]: GraphQLTypes["UpdateTenantPersonInput"];
	/** Represents an update to a `TenantPerson`. Fields that are set will be updated. */
["TenantPersonPatch"]: GraphQLTypes["TenantPersonPatch"];
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
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: GraphQLTypes["UpdateUpozorneniInput"];
	/** Represents an update to a `Upozorneni`. Fields that are set will be updated. */
["UpozorneniPatch"]: GraphQLTypes["UpozorneniPatch"];
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
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: GraphQLTypes["UpdateUpozorneniSkupinyInput"];
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: GraphQLTypes["UpozorneniSkupinyPatch"];
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
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: GraphQLTypes["UpdateUserInput"];
	/** Represents an update to a `User`. Fields that are set will be updated. */
["UserPatch"]: GraphQLTypes["UserPatch"];
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
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: GraphQLTypes["DeleteAktualityInput"];
	/** The output of our delete `Attachment` mutation. */
["DeleteAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Attachment` that was deleted by this mutation. */
	attachment?:ModelTypes["Attachment"],
	deletedAttachmentNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?:ModelTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?:ModelTypes["AttachmentsEdge"]
};
	/** All input for the `deleteAttachment` mutation. */
["DeleteAttachmentInput"]: GraphQLTypes["DeleteAttachmentInput"];
	/** The output of our delete `AttendeeExternal` mutation. */
["DeleteAttendeeExternalPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeExternal` that was deleted by this mutation. */
	attendeeExternal?:ModelTypes["AttendeeExternal"],
	deletedAttendeeExternalNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?:ModelTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?:ModelTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?:ModelTypes["AttendeeExternalsEdge"]
};
	/** All input for the `deleteAttendeeExternal` mutation. */
["DeleteAttendeeExternalInput"]: GraphQLTypes["DeleteAttendeeExternalInput"];
	/** The output of our delete `AttendeeUser` mutation. */
["DeleteAttendeeUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `AttendeeUser` that was deleted by this mutation. */
	attendeeUser?:ModelTypes["AttendeeUser"],
	deletedAttendeeUserNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?:ModelTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?:ModelTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?:ModelTypes["AttendeeUsersEdge"]
};
	/** All input for the `deleteAttendeeUser` mutation. */
["DeleteAttendeeUserInput"]: GraphQLTypes["DeleteAttendeeUserInput"];
	/** All input for the `deleteAttendeeUserByUserIdAndEventId` mutation. */
["DeleteAttendeeUserByUserIdAndEventIdInput"]: GraphQLTypes["DeleteAttendeeUserByUserIdAndEventIdInput"];
	/** The output of our delete `CohortGroup` mutation. */
["DeleteCohortGroupPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `CohortGroup` that was deleted by this mutation. */
	cohortGroup?:ModelTypes["CohortGroup"],
	deletedCohortGroupNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?:ModelTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?:ModelTypes["CohortGroupsEdge"]
};
	/** All input for the `deleteCohortGroup` mutation. */
["DeleteCohortGroupInput"]: GraphQLTypes["DeleteCohortGroupInput"];
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
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: GraphQLTypes["DeleteDokumentyInput"];
	/** The output of our delete `Event` mutation. */
["DeleteEventPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Event` that was deleted by this mutation. */
	event?:ModelTypes["Event"],
	deletedEventNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?:ModelTypes["EventsEdge"]
};
	/** All input for the `deleteEvent` mutation. */
["DeleteEventInput"]: GraphQLTypes["DeleteEventInput"];
	/** The output of our delete `FormResponse` mutation. */
["DeleteFormResponsePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `FormResponse` that was deleted by this mutation. */
	formResponse?:ModelTypes["FormResponse"],
	deletedFormResponseNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?:ModelTypes["FormResponsesEdge"]
};
	/** All input for the `deleteFormResponse` mutation. */
["DeleteFormResponseInput"]: GraphQLTypes["DeleteFormResponseInput"];
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
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: GraphQLTypes["DeleteGalerieFotoInput"];
	/** The output of our delete `Location` mutation. */
["DeleteLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Location` that was deleted by this mutation. */
	location?:ModelTypes["Location"],
	deletedLocationNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?:ModelTypes["LocationsEdge"]
};
	/** All input for the `deleteLocation` mutation. */
["DeleteLocationInput"]: GraphQLTypes["DeleteLocationInput"];
	/** The output of our delete `LocationAttachment` mutation. */
["DeleteLocationAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `LocationAttachment` that was deleted by this mutation. */
	locationAttachment?:ModelTypes["LocationAttachment"],
	deletedLocationAttachmentNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?:ModelTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?:ModelTypes["LocationAttachmentsEdge"]
};
	/** All input for the `deleteLocationAttachment` mutation. */
["DeleteLocationAttachmentInput"]: GraphQLTypes["DeleteLocationAttachmentInput"];
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
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: GraphQLTypes["DeleteNabidkaItemInput"];
	/** All input for the `deleteNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["DeleteNabidkaItemByNiPartnerAndNiIdRodicInput"]: GraphQLTypes["DeleteNabidkaItemByNiPartnerAndNiIdRodicInput"];
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?:ModelTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?:ModelTypes["PariesEdge"]
};
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
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: GraphQLTypes["DeletePermissionInput"];
	/** The output of our delete `Person` mutation. */
["DeletePersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Person` that was deleted by this mutation. */
	person?:ModelTypes["Person"],
	deletedPersonNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?:ModelTypes["PeopleEdge"]
};
	/** All input for the `deletePerson` mutation. */
["DeletePersonInput"]: GraphQLTypes["DeletePersonInput"];
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
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: GraphQLTypes["DeletePlatbyRawInput"];
	/** The output of our delete `Room` mutation. */
["DeleteRoomPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Room` that was deleted by this mutation. */
	room?:ModelTypes["Room"],
	deletedRoomNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?:ModelTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?:ModelTypes["RoomsEdge"]
};
	/** All input for the `deleteRoom` mutation. */
["DeleteRoomInput"]: GraphQLTypes["DeleteRoomInput"];
	/** The output of our delete `RoomAttachment` mutation. */
["DeleteRoomAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `RoomAttachment` that was deleted by this mutation. */
	roomAttachment?:ModelTypes["RoomAttachment"],
	deletedRoomAttachmentNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?:ModelTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?:ModelTypes["RoomAttachmentsEdge"]
};
	/** All input for the `deleteRoomAttachment` mutation. */
["DeleteRoomAttachmentInput"]: GraphQLTypes["DeleteRoomAttachmentInput"];
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
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: GraphQLTypes["DeleteRozpisItemInput"];
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?:ModelTypes["CohortGroup"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?:ModelTypes["SkupiniesEdge"]
};
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: GraphQLTypes["DeleteSkupinyInput"];
	/** The output of our delete `Tenant` mutation. */
["DeleteTenantPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `Tenant` that was deleted by this mutation. */
	tenant?:ModelTypes["Tenant"],
	deletedTenantNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?:ModelTypes["TenantsEdge"]
};
	/** All input for the `deleteTenant` mutation. */
["DeleteTenantInput"]: GraphQLTypes["DeleteTenantInput"];
	/** The output of our delete `TenantAttachment` mutation. */
["DeleteTenantAttachmentPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantAttachment` that was deleted by this mutation. */
	tenantAttachment?:ModelTypes["TenantAttachment"],
	deletedTenantAttachmentNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?:ModelTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?:ModelTypes["TenantAttachmentsEdge"]
};
	/** All input for the `deleteTenantAttachment` mutation. */
["DeleteTenantAttachmentInput"]: GraphQLTypes["DeleteTenantAttachmentInput"];
	/** The output of our delete `TenantLocation` mutation. */
["DeleteTenantLocationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantLocation` that was deleted by this mutation. */
	tenantLocation?:ModelTypes["TenantLocation"],
	deletedTenantLocationNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?:ModelTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?:ModelTypes["TenantLocationsEdge"]
};
	/** All input for the `deleteTenantLocation` mutation. */
["DeleteTenantLocationInput"]: GraphQLTypes["DeleteTenantLocationInput"];
	/** The output of our delete `TenantPerson` mutation. */
["DeleteTenantPersonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** The `TenantPerson` that was deleted by this mutation. */
	tenantPerson?:ModelTypes["TenantPerson"],
	deletedTenantPersonNodeId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?:ModelTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?:ModelTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?:ModelTypes["TenantPeopleEdge"]
};
	/** All input for the `deleteTenantPerson` mutation. */
["DeleteTenantPersonInput"]: GraphQLTypes["DeleteTenantPersonInput"];
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
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: GraphQLTypes["DeleteUserInput"];
	/** The output of our `bookLesson` mutation. */
["BookLessonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	rozpisItems?:ModelTypes["RozpisItem"][],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `bookLesson` mutation. */
["BookLessonInput"]: GraphQLTypes["BookLessonInput"];
	/** The output of our `cancelLesson` mutation. */
["CancelLessonPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	rozpisItems?:ModelTypes["RozpisItem"][],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `cancelLesson` mutation. */
["CancelLessonInput"]: GraphQLTypes["CancelLessonInput"];
	/** The output of our `cancelParticipation` mutation. */
["CancelParticipationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `cancelParticipation` mutation. */
["CancelParticipationInput"]: GraphQLTypes["CancelParticipationInput"];
	/** The output of our `changePassword` mutation. */
["ChangePasswordPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `changePassword` mutation. */
["ChangePasswordInput"]: GraphQLTypes["ChangePasswordInput"];
	/** The output of our `confirmUser` mutation. */
["ConfirmUserPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `confirmUser` mutation. */
["ConfirmUserInput"]: GraphQLTypes["ConfirmUserInput"];
	/** The output of our `createCouple` mutation. */
["CreateCouplePayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	paries?:ModelTypes["Pary"][],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `createCouple` mutation. */
["CreateCoupleInput"]: GraphQLTypes["CreateCoupleInput"];
	/** The output of our `createParticipation` mutation. */
["CreateParticipationPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `createParticipation` mutation. */
["CreateParticipationInput"]: GraphQLTypes["CreateParticipationInput"];
	/** The output of our `createParticipationExternal` mutation. */
["CreateParticipationExternalPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `createParticipationExternal` mutation. */
["CreateParticipationExternalInput"]: GraphQLTypes["CreateParticipationExternalInput"];
	/** The output of our `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	paries?:ModelTypes["Pary"][],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesInput"]: GraphQLTypes["FixUnpairedCouplesInput"];
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
	/** The output of our `prospectFormDancer` mutation. */
["ProspectFormDancerPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `prospectFormDancer` mutation. */
["ProspectFormDancerInput"]: GraphQLTypes["ProspectFormDancerInput"];
	["CrmCohort"]: GraphQLTypes["CrmCohort"];
	/** An input for mutations affecting `ProspectDatum` */
["ProspectDatumInput"]: GraphQLTypes["ProspectDatumInput"];
	/** The output of our `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	reservation?:ModelTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?:ModelTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?:ModelTypes["NabidkasEdge"]
};
	/** All input for the `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsInput"]: GraphQLTypes["ReservationSetDesiredLessonsInput"];
	/** The output of our `resetPassword` mutation. */
["ResetPasswordPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `resetPassword` mutation. */
["ResetPasswordInput"]: GraphQLTypes["ResetPasswordInput"];
	/** The output of our `submitForm` mutation. */
["SubmitFormPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `submitForm` mutation. */
["SubmitFormInput"]: GraphQLTypes["SubmitFormInput"];
	/** The output of our `verifyFunction` mutation. */
["VerifyFunctionPayload"]: {
		/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?:string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?:ModelTypes["Query"]
};
	/** All input for the `verifyFunction` mutation. */
["VerifyFunctionInput"]: GraphQLTypes["VerifyFunctionInput"];
	/** A builtin object identifier type for a function name */
["RegProc"]:any;
	/** A builtin object identifier type for a relation name */
["RegClass"]:any;
	["UploadFilePayload"]: {
		uploadUrl:string,
	objectName:string
}
    }

export type GraphQLTypes = {
    /** The root query type which gives access points into the data universe. */
["Query"]: {
	__typename: "Query",
	/** Exposes the root query type nested one level down. This is helpful for Relay 1
which can only query top level fields if they are in a particular form. */
	query: GraphQLTypes["Query"],
	/** Reads and enables pagination through a set of `Akce`. */
	akces?: GraphQLTypes["AkcesConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItems?: GraphQLTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualities?: GraphQLTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Attachment`. */
	attachments?: GraphQLTypes["AttachmentsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternals?: GraphQLTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers?: GraphQLTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `CohortGroup`. */
	cohortGroups?: GraphQLTypes["CohortGroupsConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumenties?: GraphQLTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `Event`. */
	events?: GraphQLTypes["EventsConnection"],
	/** Reads and enables pagination through a set of `FormResponse`. */
	formResponses?: GraphQLTypes["FormResponsesConnection"],
	/** Reads and enables pagination through a set of `GalerieDir`. */
	galerieDirs?: GraphQLTypes["GalerieDirsConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotos?: GraphQLTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `Location`. */
	locations?: GraphQLTypes["LocationsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachments?: GraphQLTypes["LocationAttachmentsConnection"],
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
	/** Reads and enables pagination through a set of `Person`. */
	people?: GraphQLTypes["PeopleConnection"],
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
	/** Reads and enables pagination through a set of `Room`. */
	rooms?: GraphQLTypes["RoomsConnection"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachments?: GraphQLTypes["RoomAttachmentsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpis?: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	rozpisItems?: GraphQLTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessions?: GraphQLTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupinies?: GraphQLTypes["SkupiniesConnection"],
	/** Reads and enables pagination through a set of `Tenant`. */
	tenants?: GraphQLTypes["TenantsConnection"],
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachments?: GraphQLTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations?: GraphQLTypes["TenantLocationsConnection"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople?: GraphQLTypes["TenantPeopleConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenis?: GraphQLTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupinies?: GraphQLTypes["UpozorneniSkupiniesConnection"],
	/** Reads and enables pagination through a set of `User`. */
	users?: GraphQLTypes["UsersConnection"],
	aktuality?: GraphQLTypes["Aktuality"],
	attachment?: GraphQLTypes["Attachment"],
	attendeeExternal?: GraphQLTypes["AttendeeExternal"],
	attendeeUser?: GraphQLTypes["AttendeeUser"],
	attendeeUserByUserIdAndEventId?: GraphQLTypes["AttendeeUser"],
	cohortGroup?: GraphQLTypes["CohortGroup"],
	dokumenty?: GraphQLTypes["Dokumenty"],
	event?: GraphQLTypes["Event"],
	formResponse?: GraphQLTypes["FormResponse"],
	galerieDir?: GraphQLTypes["GalerieDir"],
	galerieFoto?: GraphQLTypes["GalerieFoto"],
	location?: GraphQLTypes["Location"],
	locationAttachment?: GraphQLTypes["LocationAttachment"],
	nabidka?: GraphQLTypes["Nabidka"],
	nabidkaItem?: GraphQLTypes["NabidkaItem"],
	nabidkaItemByNiPartnerAndNiIdRodic?: GraphQLTypes["NabidkaItem"],
	page?: GraphQLTypes["Page"],
	pageByUrl?: GraphQLTypes["Page"],
	pageRevision?: GraphQLTypes["PageRevision"],
	parameter?: GraphQLTypes["Parameter"],
	pary?: GraphQLTypes["Pary"],
	paryNavrh?: GraphQLTypes["ParyNavrh"],
	permission?: GraphQLTypes["Permission"],
	person?: GraphQLTypes["Person"],
	platbyCategory?: GraphQLTypes["PlatbyCategory"],
	platbyCategoryGroup?: GraphQLTypes["PlatbyCategoryGroup"],
	platbyGroup?: GraphQLTypes["PlatbyGroup"],
	platbyGroupSkupina?: GraphQLTypes["PlatbyGroupSkupina"],
	platbyItem?: GraphQLTypes["PlatbyItem"],
	platbyRaw?: GraphQLTypes["PlatbyRaw"],
	room?: GraphQLTypes["Room"],
	roomAttachment?: GraphQLTypes["RoomAttachment"],
	rozpi?: GraphQLTypes["Rozpi"],
	rozpisItem?: GraphQLTypes["RozpisItem"],
	session?: GraphQLTypes["Session"],
	skupiny?: GraphQLTypes["Skupiny"],
	tenant?: GraphQLTypes["Tenant"],
	tenantAttachment?: GraphQLTypes["TenantAttachment"],
	tenantLocation?: GraphQLTypes["TenantLocation"],
	tenantPerson?: GraphQLTypes["TenantPerson"],
	upozorneni?: GraphQLTypes["Upozorneni"],
	upozorneniSkupiny?: GraphQLTypes["UpozorneniSkupiny"],
	user?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `Pary`. */
	activeCouples?: GraphQLTypes["PariesConnection"],
	currentCoupleIds?: GraphQLTypes["CurrentCoupleIdsConnection"],
	/** Reads and enables pagination through a set of `Permission`. */
	currentPermissions?: GraphQLTypes["PermissionsConnection"],
	currentSessionId?: string,
	currentTenantId?: GraphQLTypes["BigInt"],
	currentUserId?: GraphQLTypes["BigInt"],
	getCurrentCouple?: GraphQLTypes["Pary"],
	getCurrentTenant?: GraphQLTypes["Tenant"],
	getCurrentUser?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	myAnnouncements?: GraphQLTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `RozpisItem`. */
	myLessons?: GraphQLTypes["RozpisItemsConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	reservationsForRange?: GraphQLTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	schedulesForRange?: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Video`. */
	titleVideos?: GraphQLTypes["VideosConnection"],
	/** Reads and enables pagination through a set of `User`. */
	trainers?: GraphQLTypes["UsersConnection"]
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
	aVisible?: boolean,
	summary?: string,
	isPublic?: boolean,
	enableNotes?: boolean,
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
	aiId?: GraphQLTypes["BigInt"],
	aiIdRodic?: GraphQLTypes["BigInt"],
	aiUser?: GraphQLTypes["BigInt"],
	aiRokNarozeni?: number,
	notes?: string,
	/** Reads a single `Akce` that is related to this `AkceItem`. */
	akceByAiIdRodic?: GraphQLTypes["Akce"],
	/** Reads a single `User` that is related to this `AkceItem`. */
	userByAiUser?: GraphQLTypes["User"]
};
	["User"]: {
	__typename: "User",
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
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Permission` that is related to this `User`. */
	permissionByUGroup?: GraphQLTypes["Permission"],
	/** Reads a single `Skupiny` that is related to this `User`. */
	skupinyByUSkupina?: GraphQLTypes["Skupiny"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers: GraphQLTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtKdo: GraphQLTypes["AktualitiesConnection"],
	/** Reads and enables pagination through a set of `Dokumenty`. */
	dokumentiesByDKdo: GraphQLTypes["DokumentiesConnection"],
	/** Reads and enables pagination through a set of `GalerieFoto`. */
	galerieFotosByGfKdo: GraphQLTypes["GalerieFotosConnection"],
	/** Reads and enables pagination through a set of `PlatbyItem`. */
	platbyItemsByPiIdUser: GraphQLTypes["PlatbyItemsConnection"],
	/** Reads and enables pagination through a set of `Nabidka`. */
	nabidkasByNTrener: GraphQLTypes["NabidkasConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartner: GraphQLTypes["PariesConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnNavrhl: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartner: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `ParyNavrh`. */
	paryNavrhsByPnPartnerka: GraphQLTypes["ParyNavrhsConnection"],
	/** Reads and enables pagination through a set of `Rozpi`. */
	rozpisByRTrener: GraphQLTypes["RozpisConnection"],
	/** Reads and enables pagination through a set of `Session`. */
	sessionsBySsUser: GraphQLTypes["SessionsConnection"],
	/** Reads and enables pagination through a set of `Upozorneni`. */
	upozornenisByUpKdo: GraphQLTypes["UpozornenisConnection"],
	/** Reads and enables pagination through a set of `Attachment`. */
	attachmentsByUploadedBy: GraphQLTypes["AttachmentsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternalsByManagedBy: GraphQLTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternalsByConfirmedBy: GraphQLTypes["AttendeeExternalsConnection"],
	/** Reads and enables pagination through a set of `AkceItem`. */
	akceItemsByAiUser: GraphQLTypes["AkceItemsConnection"],
	/** Reads and enables pagination through a set of `Pary`. */
	pariesByPIdPartnerka: GraphQLTypes["PariesConnection"],
	dateOfNewestPayment?: GraphQLTypes["Date"],
	dateOfOldestPayment?: GraphQLTypes["Date"],
	fullName?: string,
	hasValidPayment?: boolean,
	inPublicCohort?: boolean
};
	["Permission"]: {
	__typename: "Permission",
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
	id?: GraphQLTypes["BigInt"],
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
	/** Checks for equality with the object’s `uJmeno` field. */
	uJmeno?: string,
	/** Checks for equality with the object’s `uPrijmeni` field. */
	uPrijmeni?: string,
	/** Checks for equality with the object’s `uNarozeni` field. */
	uNarozeni?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `uGroup` field. */
	uGroup?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uSkupina` field. */
	uSkupina?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `uBan` field. */
	uBan?: boolean,
	/** Checks for equality with the object’s `uConfirmed` field. */
	uConfirmed?: boolean,
	/** Checks for equality with the object’s `uSystem` field. */
	uSystem?: boolean,
	/** Checks for equality with the object’s `inPublicCohort` field. */
	inPublicCohort?: boolean
};
	["Skupiny"]: {
	__typename: "Skupiny",
	sId: GraphQLTypes["BigInt"],
	sName: string,
	sDescription: string,
	sColorRgb: string,
	sColorText: string,
	sLocation: string,
	sVisible: boolean,
	ordering: number,
	internalInfo: string,
	cohortGroup?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?: GraphQLTypes["CohortGroup"],
	/** Reads and enables pagination through a set of `User`. */
	usersByUSkupina: GraphQLTypes["UsersConnection"],
	/** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
	platbyGroupSkupinasByPgsIdSkupina: GraphQLTypes["PlatbyGroupSkupinasConnection"],
	/** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
	upozorneniSkupiniesByUpsIdSkupina: GraphQLTypes["UpozorneniSkupiniesConnection"]
};
	["CohortGroup"]: {
	__typename: "CohortGroup",
	id: GraphQLTypes["BigInt"],
	name: string,
	description: string,
	ordering: number,
	isPublic: boolean,
	tenant?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?: GraphQLTypes["Tenant"],
	/** Reads and enables pagination through a set of `Skupiny`. */
	skupiniesByCohortGroup: GraphQLTypes["SkupiniesConnection"]
};
	["Tenant"]: {
	__typename: "Tenant",
	id: GraphQLTypes["BigInt"],
	name: string,
	memberInfo: string,
	origins?: Array<string>,
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachments: GraphQLTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople: GraphQLTypes["TenantPeopleConnection"],
	/** Reads and enables pagination through a set of `CohortGroup`. */
	cohortGroupsByTenant: GraphQLTypes["CohortGroupsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations: GraphQLTypes["TenantLocationsConnection"]
};
	/** A connection to a list of `TenantAttachment` values. */
["TenantAttachmentsConnection"]: {
	__typename: "TenantAttachmentsConnection",
	/** A list of `TenantAttachment` objects. */
	nodes: Array<GraphQLTypes["TenantAttachment"]>,
	/** A list of edges which contains the `TenantAttachment` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["TenantAttachmentsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `TenantAttachment` you could get from the connection. */
	totalCount: number
};
	["TenantAttachment"]: {
	__typename: "TenantAttachment",
	tenantId: GraphQLTypes["BigInt"],
	objectName: string,
	type?: GraphQLTypes["TenantAttachmentType"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"]
};
	["TenantAttachmentType"]: TenantAttachmentType;
	["Attachment"]: {
	__typename: "Attachment",
	objectName: string,
	previewObjectName?: string,
	uploadedBy?: GraphQLTypes["BigInt"],
	uploadedAt: GraphQLTypes["Datetime"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `TenantAttachment`. */
	tenantAttachmentsByObjectName: GraphQLTypes["TenantAttachmentsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachmentsByObjectName: GraphQLTypes["LocationAttachmentsConnection"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachmentsByObjectName: GraphQLTypes["RoomAttachmentsConnection"]
};
	/** Methods to use when ordering `TenantAttachment`. */
["TenantAttachmentsOrderBy"]: TenantAttachmentsOrderBy;
	/** A condition to be used against `TenantAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantAttachmentCondition"]: {
		/** Checks for equality with the object’s `tenantId` field. */
	tenantId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `objectName` field. */
	objectName?: string
};
	/** A connection to a list of `LocationAttachment` values. */
["LocationAttachmentsConnection"]: {
	__typename: "LocationAttachmentsConnection",
	/** A list of `LocationAttachment` objects. */
	nodes: Array<GraphQLTypes["LocationAttachment"]>,
	/** A list of edges which contains the `LocationAttachment` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["LocationAttachmentsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `LocationAttachment` you could get from the connection. */
	totalCount: number
};
	["LocationAttachment"]: {
	__typename: "LocationAttachment",
	locationId: GraphQLTypes["BigInt"],
	objectName: string,
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?: GraphQLTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"]
};
	["Location"]: {
	__typename: "Location",
	id: GraphQLTypes["BigInt"],
	name: string,
	description: GraphQLTypes["JSON"],
	/** Reads and enables pagination through a set of `Room`. */
	roomsByLocation: GraphQLTypes["RoomsConnection"],
	/** Reads and enables pagination through a set of `LocationAttachment`. */
	locationAttachments: GraphQLTypes["LocationAttachmentsConnection"],
	/** Reads and enables pagination through a set of `TenantLocation`. */
	tenantLocations: GraphQLTypes["TenantLocationsConnection"]
};
	/** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
["JSON"]:any;
	/** A connection to a list of `Room` values. */
["RoomsConnection"]: {
	__typename: "RoomsConnection",
	/** A list of `Room` objects. */
	nodes: Array<GraphQLTypes["Room"]>,
	/** A list of edges which contains the `Room` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["RoomsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Room` you could get from the connection. */
	totalCount: number
};
	["Room"]: {
	__typename: "Room",
	id: GraphQLTypes["BigInt"],
	name: string,
	description: GraphQLTypes["JSON"],
	location?: GraphQLTypes["BigInt"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?: GraphQLTypes["Location"],
	/** Reads and enables pagination through a set of `RoomAttachment`. */
	roomAttachments: GraphQLTypes["RoomAttachmentsConnection"]
};
	/** A connection to a list of `RoomAttachment` values. */
["RoomAttachmentsConnection"]: {
	__typename: "RoomAttachmentsConnection",
	/** A list of `RoomAttachment` objects. */
	nodes: Array<GraphQLTypes["RoomAttachment"]>,
	/** A list of edges which contains the `RoomAttachment` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["RoomAttachmentsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `RoomAttachment` you could get from the connection. */
	totalCount: number
};
	["RoomAttachment"]: {
	__typename: "RoomAttachment",
	roomId: GraphQLTypes["BigInt"],
	objectName: string,
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?: GraphQLTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"]
};
	/** A `RoomAttachment` edge in the connection. */
["RoomAttachmentsEdge"]: {
	__typename: "RoomAttachmentsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `RoomAttachment` at the end of the edge. */
	node: GraphQLTypes["RoomAttachment"]
};
	/** Methods to use when ordering `RoomAttachment`. */
["RoomAttachmentsOrderBy"]: RoomAttachmentsOrderBy;
	/** A condition to be used against `RoomAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["RoomAttachmentCondition"]: {
		/** Checks for equality with the object’s `roomId` field. */
	roomId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `objectName` field. */
	objectName?: string
};
	/** A `Room` edge in the connection. */
["RoomsEdge"]: {
	__typename: "RoomsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Room` at the end of the edge. */
	node: GraphQLTypes["Room"]
};
	/** Methods to use when ordering `Room`. */
["RoomsOrderBy"]: RoomsOrderBy;
	/** A condition to be used against `Room` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["RoomCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `location` field. */
	location?: GraphQLTypes["BigInt"]
};
	/** Methods to use when ordering `LocationAttachment`. */
["LocationAttachmentsOrderBy"]: LocationAttachmentsOrderBy;
	/** A condition to be used against `LocationAttachment` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["LocationAttachmentCondition"]: {
		/** Checks for equality with the object’s `locationId` field. */
	locationId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `objectName` field. */
	objectName?: string
};
	/** A connection to a list of `TenantLocation` values. */
["TenantLocationsConnection"]: {
	__typename: "TenantLocationsConnection",
	/** A list of `TenantLocation` objects. */
	nodes: Array<GraphQLTypes["TenantLocation"]>,
	/** A list of edges which contains the `TenantLocation` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["TenantLocationsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `TenantLocation` you could get from the connection. */
	totalCount: number
};
	["TenantLocation"]: {
	__typename: "TenantLocation",
	tenantId: GraphQLTypes["BigInt"],
	locationId: GraphQLTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?: GraphQLTypes["Location"]
};
	/** A `TenantLocation` edge in the connection. */
["TenantLocationsEdge"]: {
	__typename: "TenantLocationsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `TenantLocation` at the end of the edge. */
	node: GraphQLTypes["TenantLocation"]
};
	/** Methods to use when ordering `TenantLocation`. */
["TenantLocationsOrderBy"]: TenantLocationsOrderBy;
	/** A condition to be used against `TenantLocation` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantLocationCondition"]: {
		/** Checks for equality with the object’s `tenantId` field. */
	tenantId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `locationId` field. */
	locationId?: GraphQLTypes["BigInt"]
};
	/** A `LocationAttachment` edge in the connection. */
["LocationAttachmentsEdge"]: {
	__typename: "LocationAttachmentsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `LocationAttachment` at the end of the edge. */
	node: GraphQLTypes["LocationAttachment"]
};
	/** A `TenantAttachment` edge in the connection. */
["TenantAttachmentsEdge"]: {
	__typename: "TenantAttachmentsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `TenantAttachment` at the end of the edge. */
	node: GraphQLTypes["TenantAttachment"]
};
	/** A connection to a list of `TenantPerson` values. */
["TenantPeopleConnection"]: {
	__typename: "TenantPeopleConnection",
	/** A list of `TenantPerson` objects. */
	nodes: Array<GraphQLTypes["TenantPerson"]>,
	/** A list of edges which contains the `TenantPerson` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["TenantPeopleEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `TenantPerson` you could get from the connection. */
	totalCount: number
};
	["TenantPerson"]: {
	__typename: "TenantPerson",
	tenantId: GraphQLTypes["BigInt"],
	personId: GraphQLTypes["BigInt"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?: GraphQLTypes["Person"]
};
	["Person"]: {
	__typename: "Person",
	id: GraphQLTypes["BigInt"],
	firstName: string,
	lastName: string,
	gender: GraphQLTypes["GenderType"],
	/** Reads and enables pagination through a set of `TenantPerson`. */
	tenantPeople: GraphQLTypes["TenantPeopleConnection"]
};
	["GenderType"]: GenderType;
	/** Methods to use when ordering `TenantPerson`. */
["TenantPeopleOrderBy"]: TenantPeopleOrderBy;
	/** A condition to be used against `TenantPerson` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["TenantPersonCondition"]: {
		/** Checks for equality with the object’s `tenantId` field. */
	tenantId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `personId` field. */
	personId?: GraphQLTypes["BigInt"]
};
	/** A `TenantPerson` edge in the connection. */
["TenantPeopleEdge"]: {
	__typename: "TenantPeopleEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `TenantPerson` at the end of the edge. */
	node: GraphQLTypes["TenantPerson"]
};
	/** A connection to a list of `CohortGroup` values. */
["CohortGroupsConnection"]: {
	__typename: "CohortGroupsConnection",
	/** A list of `CohortGroup` objects. */
	nodes: Array<GraphQLTypes["CohortGroup"]>,
	/** A list of edges which contains the `CohortGroup` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["CohortGroupsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `CohortGroup` you could get from the connection. */
	totalCount: number
};
	/** A `CohortGroup` edge in the connection. */
["CohortGroupsEdge"]: {
	__typename: "CohortGroupsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `CohortGroup` at the end of the edge. */
	node: GraphQLTypes["CohortGroup"]
};
	/** Methods to use when ordering `CohortGroup`. */
["CohortGroupsOrderBy"]: CohortGroupsOrderBy;
	/** A condition to be used against `CohortGroup` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["CohortGroupCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `ordering` field. */
	ordering?: number,
	/** Checks for equality with the object’s `isPublic` field. */
	isPublic?: boolean,
	/** Checks for equality with the object’s `tenant` field. */
	tenant?: GraphQLTypes["BigInt"]
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
	/** Checks for equality with the object’s `sVisible` field. */
	sVisible?: boolean,
	/** Checks for equality with the object’s `ordering` field. */
	ordering?: number,
	/** Checks for equality with the object’s `cohortGroup` field. */
	cohortGroup?: GraphQLTypes["BigInt"]
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
	pgsId: GraphQLTypes["BigInt"],
	pgsIdSkupina: GraphQLTypes["BigInt"],
	pgsIdGroup: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
	skupinyByPgsIdSkupina?: GraphQLTypes["Skupiny"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
	platbyGroupByPgsIdGroup?: GraphQLTypes["PlatbyGroup"]
};
	["PlatbyGroup"]: {
	__typename: "PlatbyGroup",
	pgId: GraphQLTypes["BigInt"],
	pgType: GraphQLTypes["BigFloat"],
	pgName: string,
	pgDescription: string,
	pgBase: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	pcgId: GraphQLTypes["BigInt"],
	pcgIdGroup: GraphQLTypes["BigInt"],
	pcgIdCategory: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
	platbyGroupByPcgIdGroup?: GraphQLTypes["PlatbyGroup"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
	platbyCategoryByPcgIdCategory?: GraphQLTypes["PlatbyCategory"]
};
	["PlatbyCategory"]: {
	__typename: "PlatbyCategory",
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
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	piId: GraphQLTypes["BigInt"],
	piIdUser?: GraphQLTypes["BigInt"],
	piIdCategory: GraphQLTypes["BigInt"],
	piIdRaw?: GraphQLTypes["BigInt"],
	piAmount: GraphQLTypes["BigFloat"],
	piDate: GraphQLTypes["Date"],
	piPrefix: number,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `PlatbyItem`. */
	userByPiIdUser?: GraphQLTypes["User"],
	/** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
	platbyCategoryByPiIdCategory?: GraphQLTypes["PlatbyCategory"],
	/** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
	platbyRawByPiIdRaw?: GraphQLTypes["PlatbyRaw"]
};
	["PlatbyRaw"]: {
	__typename: "PlatbyRaw",
	prId: GraphQLTypes["BigInt"],
	prRaw: string,
	prHash: string,
	prSorted: boolean,
	prDiscarded: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	piIdRaw?: GraphQLTypes["BigInt"]
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
	upsId: GraphQLTypes["BigInt"],
	upsIdRodic: GraphQLTypes["BigInt"],
	upsIdSkupina: GraphQLTypes["BigInt"],
	upsColor: string,
	upsPopis: string,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
	upozorneniByUpsIdRodic?: GraphQLTypes["Upozorneni"],
	/** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
	skupinyByUpsIdSkupina?: GraphQLTypes["Skupiny"]
};
	["Upozorneni"]: {
	__typename: "Upozorneni",
	upId: GraphQLTypes["BigInt"],
	upKdo?: GraphQLTypes["BigInt"],
	upNadpis: string,
	upText: string,
	upBarvy: GraphQLTypes["BigInt"],
	upLock: boolean,
	upTimestamp?: GraphQLTypes["Datetime"],
	upTimestampAdd: GraphQLTypes["Datetime"],
	scheduledSince?: GraphQLTypes["Datetime"],
	scheduledUntil?: GraphQLTypes["Datetime"],
	isVisible?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	upsIdSkupina?: GraphQLTypes["BigInt"]
};
	/** A `UpozorneniSkupiny` edge in the connection. */
["UpozorneniSkupiniesEdge"]: {
	__typename: "UpozorneniSkupiniesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `UpozorneniSkupiny` at the end of the edge. */
	node: GraphQLTypes["UpozorneniSkupiny"]
};
	/** A connection to a list of `AttendeeUser` values. */
["AttendeeUsersConnection"]: {
	__typename: "AttendeeUsersConnection",
	/** A list of `AttendeeUser` objects. */
	nodes: Array<GraphQLTypes["AttendeeUser"]>,
	/** A list of edges which contains the `AttendeeUser` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AttendeeUsersEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `AttendeeUser` you could get from the connection. */
	totalCount: number
};
	["AttendeeUser"]: {
	__typename: "AttendeeUser",
	id: GraphQLTypes["BigInt"],
	eventId: GraphQLTypes["BigInt"],
	userId: GraphQLTypes["BigInt"],
	birthYear: number,
	notes: string,
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?: GraphQLTypes["User"]
};
	["Event"]: {
	__typename: "Event",
	id: GraphQLTypes["BigInt"],
	name: string,
	locationText: string,
	description: string,
	since: GraphQLTypes["Date"],
	until: GraphQLTypes["Date"],
	capacity: GraphQLTypes["BigInt"],
	filesLegacy: string,
	updatedAt?: GraphQLTypes["Datetime"],
	isLocked: boolean,
	isVisible: boolean,
	summary: string,
	isPublic: boolean,
	enableNotes: boolean,
	tenantId: GraphQLTypes["BigInt"],
	/** Reads and enables pagination through a set of `AttendeeUser`. */
	attendeeUsers: GraphQLTypes["AttendeeUsersConnection"],
	/** Reads and enables pagination through a set of `AttendeeExternal`. */
	attendeeExternals: GraphQLTypes["AttendeeExternalsConnection"],
	remainingSpots?: number
};
	/** Methods to use when ordering `AttendeeUser`. */
["AttendeeUsersOrderBy"]: AttendeeUsersOrderBy;
	/** A condition to be used against `AttendeeUser` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeUserCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `eventId` field. */
	eventId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `userId` field. */
	userId?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `AttendeeExternal` values. */
["AttendeeExternalsConnection"]: {
	__typename: "AttendeeExternalsConnection",
	/** A list of `AttendeeExternal` objects. */
	nodes: Array<GraphQLTypes["AttendeeExternal"]>,
	/** A list of edges which contains the `AttendeeExternal` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AttendeeExternalsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `AttendeeExternal` you could get from the connection. */
	totalCount: number
};
	["AttendeeExternal"]: {
	__typename: "AttendeeExternal",
	id: GraphQLTypes["BigInt"],
	eventId: GraphQLTypes["BigInt"],
	firstName: string,
	lastName: string,
	email: string,
	phone: string,
	notes: string,
	birthNumber?: string,
	guardianName: string,
	managedBy?: GraphQLTypes["BigInt"],
	confirmedBy?: GraphQLTypes["BigInt"],
	confirmedAt?: GraphQLTypes["Datetime"],
	createdAt: GraphQLTypes["Datetime"],
	updatedAt: GraphQLTypes["Datetime"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?: GraphQLTypes["User"]
};
	/** A `AttendeeExternal` edge in the connection. */
["AttendeeExternalsEdge"]: {
	__typename: "AttendeeExternalsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `AttendeeExternal` at the end of the edge. */
	node: GraphQLTypes["AttendeeExternal"]
};
	/** Methods to use when ordering `AttendeeExternal`. */
["AttendeeExternalsOrderBy"]: AttendeeExternalsOrderBy;
	/** A condition to be used against `AttendeeExternal` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["AttendeeExternalCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `eventId` field. */
	eventId?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `managedBy` field. */
	managedBy?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `confirmedBy` field. */
	confirmedBy?: GraphQLTypes["BigInt"]
};
	/** A `AttendeeUser` edge in the connection. */
["AttendeeUsersEdge"]: {
	__typename: "AttendeeUsersEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `AttendeeUser` at the end of the edge. */
	node: GraphQLTypes["AttendeeUser"]
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
	atId: GraphQLTypes["BigInt"],
	atKdo?: GraphQLTypes["BigInt"],
	atKat: string,
	atJmeno: string,
	atText: string,
	atPreview: string,
	atFoto?: GraphQLTypes["BigInt"],
	atFotoMain?: GraphQLTypes["BigInt"],
	atTimestamp?: GraphQLTypes["Datetime"],
	atTimestampAdd?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `Aktuality`. */
	userByAtKdo?: GraphQLTypes["User"],
	/** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
	galerieFotoByAtFotoMain?: GraphQLTypes["GalerieFoto"]
};
	["GalerieFoto"]: {
	__typename: "GalerieFoto",
	gfId: GraphQLTypes["BigInt"],
	gfIdRodic: GraphQLTypes["BigInt"],
	gfName: string,
	gfPath: string,
	gfKdo: GraphQLTypes["BigInt"],
	gfTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
	galerieDirByGfIdRodic?: GraphQLTypes["GalerieDir"],
	/** Reads a single `User` that is related to this `GalerieFoto`. */
	userByGfKdo?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `Aktuality`. */
	aktualitiesByAtFotoMain: GraphQLTypes["AktualitiesConnection"]
};
	["GalerieDir"]: {
	__typename: "GalerieDir",
	gdId: GraphQLTypes["BigInt"],
	gdIdRodic: GraphQLTypes["BigInt"],
	gdName: string,
	gdLevel: number,
	gdPath: string,
	gdHidden: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	/** Checks for equality with the object’s `gfKdo` field. */
	gfKdo?: GraphQLTypes["BigInt"]
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
	/** Checks for equality with the object’s `atFotoMain` field. */
	atFotoMain?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `atTimestampAdd` field. */
	atTimestampAdd?: GraphQLTypes["Datetime"],
	/** Checks for equality with the object’s `tenantId` field. */
	tenantId?: GraphQLTypes["BigInt"]
};
	/** A `Aktuality` edge in the connection. */
["AktualitiesEdge"]: {
	__typename: "AktualitiesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Aktuality` at the end of the edge. */
	node: GraphQLTypes["Aktuality"]
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
	dId: GraphQLTypes["BigInt"],
	dPath: string,
	dName: string,
	dFilename: string,
	dKategorie: number,
	dKdo: GraphQLTypes["BigInt"],
	dTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	/** Checks for equality with the object’s `dKategorie` field. */
	dKategorie?: number,
	/** Checks for equality with the object’s `dKdo` field. */
	dKdo?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `dTimestamp` field. */
	dTimestamp?: GraphQLTypes["Datetime"]
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
	nId: GraphQLTypes["BigInt"],
	nTrener: GraphQLTypes["BigInt"],
	nPocetHod: number,
	nMaxPocetHod: number,
	nOd: GraphQLTypes["Date"],
	nDo: GraphQLTypes["Date"],
	nVisible: boolean,
	nLock: boolean,
	nTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** Reads and enables pagination through a set of `NabidkaItem`. */
	nabidkaItemsByNiIdRodic: GraphQLTypes["NabidkaItemsConnection"],
	freeLessons?: number,
	myLessons?: number
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
	niId: GraphQLTypes["BigInt"],
	niIdRodic: GraphQLTypes["BigInt"],
	niPartner: GraphQLTypes["BigInt"],
	niPocetHod: number,
	niLock: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
	nabidkaByNiIdRodic?: GraphQLTypes["Nabidka"],
	/** Reads a single `Pary` that is related to this `NabidkaItem`. */
	paryByNiPartner?: GraphQLTypes["Pary"]
};
	["Pary"]: {
	__typename: "Pary",
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
	id?: GraphQLTypes["BigInt"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartner?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?: GraphQLTypes["User"],
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
	niPartner?: GraphQLTypes["BigInt"]
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
	riId: GraphQLTypes["BigInt"],
	riIdRodic: GraphQLTypes["BigInt"],
	riPartner?: GraphQLTypes["BigInt"],
	riOd: GraphQLTypes["Time"],
	riDo: GraphQLTypes["Time"],
	riLock: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
	/** Reads a single `Rozpi` that is related to this `RozpisItem`. */
	rozpiByRiIdRodic?: GraphQLTypes["Rozpi"],
	/** Reads a single `Pary` that is related to this `RozpisItem`. */
	paryByRiPartner?: GraphQLTypes["Pary"]
};
	/** The exact time of day, does not include the date. May or may not have a timezone offset. */
["Time"]:any;
	["Rozpi"]: {
	__typename: "Rozpi",
	rId: GraphQLTypes["BigInt"],
	rTrener: GraphQLTypes["BigInt"],
	rKde: string,
	rDatum: GraphQLTypes["Date"],
	rVisible: boolean,
	rLock: boolean,
	rTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId: GraphQLTypes["BigInt"],
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
	riOd?: GraphQLTypes["Time"]
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
	/** Checks for equality with the object’s `nOd` field. */
	nOd?: GraphQLTypes["Date"]
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
	/** Checks for equality with the object’s `pHodnoceni` field. */
	pHodnoceni?: number
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
	pnId: GraphQLTypes["BigInt"],
	pnNavrhl: GraphQLTypes["BigInt"],
	pnPartner: GraphQLTypes["BigInt"],
	pnPartnerka: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
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
	/** Checks for equality with the object’s `rDatum` field. */
	rDatum?: GraphQLTypes["Date"]
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
	ssId: string,
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
	/** Checks for equality with the object’s `ssUser` field. */
	ssUser?: GraphQLTypes["BigInt"]
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
	/** Checks for equality with the object’s `upTimestampAdd` field. */
	upTimestampAdd?: GraphQLTypes["Datetime"]
};
	/** A connection to a list of `Attachment` values. */
["AttachmentsConnection"]: {
	__typename: "AttachmentsConnection",
	/** A list of `Attachment` objects. */
	nodes: Array<GraphQLTypes["Attachment"]>,
	/** A list of edges which contains the `Attachment` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["AttachmentsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Attachment` you could get from the connection. */
	totalCount: number
};
	/** A `Attachment` edge in the connection. */
["AttachmentsEdge"]: {
	__typename: "AttachmentsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Attachment` at the end of the edge. */
	node: GraphQLTypes["Attachment"]
};
	/** Methods to use when ordering `Attachment`. */
["AttachmentsOrderBy"]: AttachmentsOrderBy;
	/** A condition to be used against `Attachment` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["AttachmentCondition"]: {
		/** Checks for equality with the object’s `objectName` field. */
	objectName?: string,
	/** Checks for equality with the object’s `uploadedBy` field. */
	uploadedBy?: GraphQLTypes["BigInt"]
};
	/** Methods to use when ordering `AkceItem`. */
["AkceItemsOrderBy"]: AkceItemsOrderBy;
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
	/** A connection to a list of `Event` values. */
["EventsConnection"]: {
	__typename: "EventsConnection",
	/** A list of `Event` objects. */
	nodes: Array<GraphQLTypes["Event"]>,
	/** A list of edges which contains the `Event` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["EventsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Event` you could get from the connection. */
	totalCount: number
};
	/** A `Event` edge in the connection. */
["EventsEdge"]: {
	__typename: "EventsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Event` at the end of the edge. */
	node: GraphQLTypes["Event"]
};
	/** Methods to use when ordering `Event`. */
["EventsOrderBy"]: EventsOrderBy;
	/** A condition to be used against `Event` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["EventCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `since` field. */
	since?: GraphQLTypes["Date"],
	/** Checks for equality with the object’s `isVisible` field. */
	isVisible?: boolean
};
	/** A connection to a list of `FormResponse` values. */
["FormResponsesConnection"]: {
	__typename: "FormResponsesConnection",
	/** A list of `FormResponse` objects. */
	nodes: Array<GraphQLTypes["FormResponse"]>,
	/** A list of edges which contains the `FormResponse` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["FormResponsesEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `FormResponse` you could get from the connection. */
	totalCount: number
};
	["FormResponse"]: {
	__typename: "FormResponse",
	id: GraphQLTypes["BigInt"],
	type: string,
	data: GraphQLTypes["JSON"],
	url: string,
	createdAt: GraphQLTypes["Datetime"],
	updatedAt: GraphQLTypes["Datetime"],
	tenantId: GraphQLTypes["BigInt"]
};
	/** A `FormResponse` edge in the connection. */
["FormResponsesEdge"]: {
	__typename: "FormResponsesEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `FormResponse` at the end of the edge. */
	node: GraphQLTypes["FormResponse"]
};
	/** Methods to use when ordering `FormResponse`. */
["FormResponsesOrderBy"]: FormResponsesOrderBy;
	/** A condition to be used against `FormResponse` object types. All fields are
tested for equality and combined with a logical ‘and.’ */
["FormResponseCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"],
	/** Checks for equality with the object’s `type` field. */
	type?: string,
	/** Checks for equality with the object’s `updatedAt` field. */
	updatedAt?: GraphQLTypes["Datetime"]
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
	gdIdRodic?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `Location` values. */
["LocationsConnection"]: {
	__typename: "LocationsConnection",
	/** A list of `Location` objects. */
	nodes: Array<GraphQLTypes["Location"]>,
	/** A list of edges which contains the `Location` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["LocationsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Location` you could get from the connection. */
	totalCount: number
};
	/** A `Location` edge in the connection. */
["LocationsEdge"]: {
	__typename: "LocationsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Location` at the end of the edge. */
	node: GraphQLTypes["Location"]
};
	/** Methods to use when ordering `Location`. */
["LocationsOrderBy"]: LocationsOrderBy;
	/** A condition to be used against `Location` object types. All fields are tested
for equality and combined with a logical ‘and.’ */
["LocationCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"]
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
	id: number,
	url: string,
	content: GraphQLTypes["JSON"],
	createdAt: GraphQLTypes["Datetime"],
	updatedAt: GraphQLTypes["Datetime"],
	title: string
};
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
	url?: string
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
	revNumber?: number
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
	paName?: string
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
	peId?: GraphQLTypes["BigInt"]
};
	/** A connection to a list of `Person` values. */
["PeopleConnection"]: {
	__typename: "PeopleConnection",
	/** A list of `Person` objects. */
	nodes: Array<GraphQLTypes["Person"]>,
	/** A list of edges which contains the `Person` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["PeopleEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Person` you could get from the connection. */
	totalCount: number
};
	/** A `Person` edge in the connection. */
["PeopleEdge"]: {
	__typename: "PeopleEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Person` at the end of the edge. */
	node: GraphQLTypes["Person"]
};
	/** Methods to use when ordering `Person`. */
["PeopleOrderBy"]: PeopleOrderBy;
	/** A condition to be used against `Person` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["PersonCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"]
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
	/** Checks for equality with the object’s `pcSymbol` field. */
	pcSymbol?: GraphQLTypes["BigInt"]
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
	pgId?: GraphQLTypes["BigInt"]
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
	/** Checks for equality with the object’s `prHash` field. */
	prHash?: string
};
	/** A connection to a list of `Tenant` values. */
["TenantsConnection"]: {
	__typename: "TenantsConnection",
	/** A list of `Tenant` objects. */
	nodes: Array<GraphQLTypes["Tenant"]>,
	/** A list of edges which contains the `Tenant` and cursor to aid in pagination. */
	edges: Array<GraphQLTypes["TenantsEdge"]>,
	/** Information to aid in pagination. */
	pageInfo: GraphQLTypes["PageInfo"],
	/** The count of *all* `Tenant` you could get from the connection. */
	totalCount: number
};
	/** A `Tenant` edge in the connection. */
["TenantsEdge"]: {
	__typename: "TenantsEdge",
	/** A cursor for use in pagination. */
	cursor?: GraphQLTypes["Cursor"],
	/** The `Tenant` at the end of the edge. */
	node: GraphQLTypes["Tenant"]
};
	/** Methods to use when ordering `Tenant`. */
["TenantsOrderBy"]: TenantsOrderBy;
	/** A condition to be used against `Tenant` object types. All fields are tested for equality and combined with a logical ‘and.’ */
["TenantCondition"]: {
		/** Checks for equality with the object’s `id` field. */
	id?: GraphQLTypes["BigInt"]
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
	/** The root mutation type which contains root level fields which mutate data. */
["Mutation"]: {
	__typename: "Mutation",
	/** Creates a single `Akce`. */
	createAkce?: GraphQLTypes["CreateAkcePayload"],
	/** Creates a single `AkceItem`. */
	createAkceItem?: GraphQLTypes["CreateAkceItemPayload"],
	/** Creates a single `Aktuality`. */
	createAktuality?: GraphQLTypes["CreateAktualityPayload"],
	/** Creates a single `Attachment`. */
	createAttachment?: GraphQLTypes["CreateAttachmentPayload"],
	/** Creates a single `AttendeeExternal`. */
	createAttendeeExternal?: GraphQLTypes["CreateAttendeeExternalPayload"],
	/** Creates a single `AttendeeUser`. */
	createAttendeeUser?: GraphQLTypes["CreateAttendeeUserPayload"],
	/** Creates a single `CohortGroup`. */
	createCohortGroup?: GraphQLTypes["CreateCohortGroupPayload"],
	/** Creates a single `Dokumenty`. */
	createDokumenty?: GraphQLTypes["CreateDokumentyPayload"],
	/** Creates a single `Event`. */
	createEvent?: GraphQLTypes["CreateEventPayload"],
	/** Creates a single `FormResponse`. */
	createFormResponse?: GraphQLTypes["CreateFormResponsePayload"],
	/** Creates a single `GalerieDir`. */
	createGalerieDir?: GraphQLTypes["CreateGalerieDirPayload"],
	/** Creates a single `GalerieFoto`. */
	createGalerieFoto?: GraphQLTypes["CreateGalerieFotoPayload"],
	/** Creates a single `Location`. */
	createLocation?: GraphQLTypes["CreateLocationPayload"],
	/** Creates a single `LocationAttachment`. */
	createLocationAttachment?: GraphQLTypes["CreateLocationAttachmentPayload"],
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
	/** Creates a single `Person`. */
	createPerson?: GraphQLTypes["CreatePersonPayload"],
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
	/** Creates a single `Room`. */
	createRoom?: GraphQLTypes["CreateRoomPayload"],
	/** Creates a single `RoomAttachment`. */
	createRoomAttachment?: GraphQLTypes["CreateRoomAttachmentPayload"],
	/** Creates a single `Rozpi`. */
	createRozpi?: GraphQLTypes["CreateRozpiPayload"],
	/** Creates a single `RozpisItem`. */
	createRozpisItem?: GraphQLTypes["CreateRozpisItemPayload"],
	/** Creates a single `Skupiny`. */
	createSkupiny?: GraphQLTypes["CreateSkupinyPayload"],
	/** Creates a single `Tenant`. */
	createTenant?: GraphQLTypes["CreateTenantPayload"],
	/** Creates a single `TenantAttachment`. */
	createTenantAttachment?: GraphQLTypes["CreateTenantAttachmentPayload"],
	/** Creates a single `TenantLocation`. */
	createTenantLocation?: GraphQLTypes["CreateTenantLocationPayload"],
	/** Creates a single `TenantPerson`. */
	createTenantPerson?: GraphQLTypes["CreateTenantPersonPayload"],
	/** Creates a single `Upozorneni`. */
	createUpozorneni?: GraphQLTypes["CreateUpozorneniPayload"],
	/** Creates a single `UpozorneniSkupiny`. */
	createUpozorneniSkupiny?: GraphQLTypes["CreateUpozorneniSkupinyPayload"],
	/** Creates a single `User`. */
	createUser?: GraphQLTypes["CreateUserPayload"],
	/** Updates a single `Aktuality` using a unique key and a patch. */
	updateAktuality?: GraphQLTypes["UpdateAktualityPayload"],
	/** Updates a single `Attachment` using a unique key and a patch. */
	updateAttachment?: GraphQLTypes["UpdateAttachmentPayload"],
	/** Updates a single `AttendeeExternal` using a unique key and a patch. */
	updateAttendeeExternal?: GraphQLTypes["UpdateAttendeeExternalPayload"],
	/** Updates a single `AttendeeUser` using a unique key and a patch. */
	updateAttendeeUser?: GraphQLTypes["UpdateAttendeeUserPayload"],
	/** Updates a single `AttendeeUser` using a unique key and a patch. */
	updateAttendeeUserByUserIdAndEventId?: GraphQLTypes["UpdateAttendeeUserPayload"],
	/** Updates a single `CohortGroup` using a unique key and a patch. */
	updateCohortGroup?: GraphQLTypes["UpdateCohortGroupPayload"],
	/** Updates a single `Dokumenty` using a unique key and a patch. */
	updateDokumenty?: GraphQLTypes["UpdateDokumentyPayload"],
	/** Updates a single `Event` using a unique key and a patch. */
	updateEvent?: GraphQLTypes["UpdateEventPayload"],
	/** Updates a single `FormResponse` using a unique key and a patch. */
	updateFormResponse?: GraphQLTypes["UpdateFormResponsePayload"],
	/** Updates a single `GalerieDir` using a unique key and a patch. */
	updateGalerieDir?: GraphQLTypes["UpdateGalerieDirPayload"],
	/** Updates a single `GalerieFoto` using a unique key and a patch. */
	updateGalerieFoto?: GraphQLTypes["UpdateGalerieFotoPayload"],
	/** Updates a single `Location` using a unique key and a patch. */
	updateLocation?: GraphQLTypes["UpdateLocationPayload"],
	/** Updates a single `LocationAttachment` using a unique key and a patch. */
	updateLocationAttachment?: GraphQLTypes["UpdateLocationAttachmentPayload"],
	/** Updates a single `Nabidka` using a unique key and a patch. */
	updateNabidka?: GraphQLTypes["UpdateNabidkaPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItem?: GraphQLTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `NabidkaItem` using a unique key and a patch. */
	updateNabidkaItemByNiPartnerAndNiIdRodic?: GraphQLTypes["UpdateNabidkaItemPayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePage?: GraphQLTypes["UpdatePagePayload"],
	/** Updates a single `Page` using a unique key and a patch. */
	updatePageByUrl?: GraphQLTypes["UpdatePagePayload"],
	/** Updates a single `Parameter` using a unique key and a patch. */
	updateParameter?: GraphQLTypes["UpdateParameterPayload"],
	/** Updates a single `Pary` using a unique key and a patch. */
	updatePary?: GraphQLTypes["UpdateParyPayload"],
	/** Updates a single `ParyNavrh` using a unique key and a patch. */
	updateParyNavrh?: GraphQLTypes["UpdateParyNavrhPayload"],
	/** Updates a single `Permission` using a unique key and a patch. */
	updatePermission?: GraphQLTypes["UpdatePermissionPayload"],
	/** Updates a single `Person` using a unique key and a patch. */
	updatePerson?: GraphQLTypes["UpdatePersonPayload"],
	/** Updates a single `PlatbyCategory` using a unique key and a patch. */
	updatePlatbyCategory?: GraphQLTypes["UpdatePlatbyCategoryPayload"],
	/** Updates a single `PlatbyCategoryGroup` using a unique key and a patch. */
	updatePlatbyCategoryGroup?: GraphQLTypes["UpdatePlatbyCategoryGroupPayload"],
	/** Updates a single `PlatbyGroup` using a unique key and a patch. */
	updatePlatbyGroup?: GraphQLTypes["UpdatePlatbyGroupPayload"],
	/** Updates a single `PlatbyGroupSkupina` using a unique key and a patch. */
	updatePlatbyGroupSkupina?: GraphQLTypes["UpdatePlatbyGroupSkupinaPayload"],
	/** Updates a single `PlatbyItem` using a unique key and a patch. */
	updatePlatbyItem?: GraphQLTypes["UpdatePlatbyItemPayload"],
	/** Updates a single `PlatbyRaw` using a unique key and a patch. */
	updatePlatbyRaw?: GraphQLTypes["UpdatePlatbyRawPayload"],
	/** Updates a single `Room` using a unique key and a patch. */
	updateRoom?: GraphQLTypes["UpdateRoomPayload"],
	/** Updates a single `RoomAttachment` using a unique key and a patch. */
	updateRoomAttachment?: GraphQLTypes["UpdateRoomAttachmentPayload"],
	/** Updates a single `Rozpi` using a unique key and a patch. */
	updateRozpi?: GraphQLTypes["UpdateRozpiPayload"],
	/** Updates a single `RozpisItem` using a unique key and a patch. */
	updateRozpisItem?: GraphQLTypes["UpdateRozpisItemPayload"],
	/** Updates a single `Skupiny` using a unique key and a patch. */
	updateSkupiny?: GraphQLTypes["UpdateSkupinyPayload"],
	/** Updates a single `Tenant` using a unique key and a patch. */
	updateTenant?: GraphQLTypes["UpdateTenantPayload"],
	/** Updates a single `TenantAttachment` using a unique key and a patch. */
	updateTenantAttachment?: GraphQLTypes["UpdateTenantAttachmentPayload"],
	/** Updates a single `TenantLocation` using a unique key and a patch. */
	updateTenantLocation?: GraphQLTypes["UpdateTenantLocationPayload"],
	/** Updates a single `TenantPerson` using a unique key and a patch. */
	updateTenantPerson?: GraphQLTypes["UpdateTenantPersonPayload"],
	/** Updates a single `Upozorneni` using a unique key and a patch. */
	updateUpozorneni?: GraphQLTypes["UpdateUpozorneniPayload"],
	/** Updates a single `UpozorneniSkupiny` using a unique key and a patch. */
	updateUpozorneniSkupiny?: GraphQLTypes["UpdateUpozorneniSkupinyPayload"],
	/** Updates a single `User` using a unique key and a patch. */
	updateUser?: GraphQLTypes["UpdateUserPayload"],
	/** Deletes a single `Aktuality` using a unique key. */
	deleteAktuality?: GraphQLTypes["DeleteAktualityPayload"],
	/** Deletes a single `Attachment` using a unique key. */
	deleteAttachment?: GraphQLTypes["DeleteAttachmentPayload"],
	/** Deletes a single `AttendeeExternal` using a unique key. */
	deleteAttendeeExternal?: GraphQLTypes["DeleteAttendeeExternalPayload"],
	/** Deletes a single `AttendeeUser` using a unique key. */
	deleteAttendeeUser?: GraphQLTypes["DeleteAttendeeUserPayload"],
	/** Deletes a single `AttendeeUser` using a unique key. */
	deleteAttendeeUserByUserIdAndEventId?: GraphQLTypes["DeleteAttendeeUserPayload"],
	/** Deletes a single `CohortGroup` using a unique key. */
	deleteCohortGroup?: GraphQLTypes["DeleteCohortGroupPayload"],
	/** Deletes a single `Dokumenty` using a unique key. */
	deleteDokumenty?: GraphQLTypes["DeleteDokumentyPayload"],
	/** Deletes a single `Event` using a unique key. */
	deleteEvent?: GraphQLTypes["DeleteEventPayload"],
	/** Deletes a single `FormResponse` using a unique key. */
	deleteFormResponse?: GraphQLTypes["DeleteFormResponsePayload"],
	/** Deletes a single `GalerieDir` using a unique key. */
	deleteGalerieDir?: GraphQLTypes["DeleteGalerieDirPayload"],
	/** Deletes a single `GalerieFoto` using a unique key. */
	deleteGalerieFoto?: GraphQLTypes["DeleteGalerieFotoPayload"],
	/** Deletes a single `Location` using a unique key. */
	deleteLocation?: GraphQLTypes["DeleteLocationPayload"],
	/** Deletes a single `LocationAttachment` using a unique key. */
	deleteLocationAttachment?: GraphQLTypes["DeleteLocationAttachmentPayload"],
	/** Deletes a single `Nabidka` using a unique key. */
	deleteNabidka?: GraphQLTypes["DeleteNabidkaPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItem?: GraphQLTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `NabidkaItem` using a unique key. */
	deleteNabidkaItemByNiPartnerAndNiIdRodic?: GraphQLTypes["DeleteNabidkaItemPayload"],
	/** Deletes a single `Parameter` using a unique key. */
	deleteParameter?: GraphQLTypes["DeleteParameterPayload"],
	/** Deletes a single `Pary` using a unique key. */
	deletePary?: GraphQLTypes["DeleteParyPayload"],
	/** Deletes a single `ParyNavrh` using a unique key. */
	deleteParyNavrh?: GraphQLTypes["DeleteParyNavrhPayload"],
	/** Deletes a single `Permission` using a unique key. */
	deletePermission?: GraphQLTypes["DeletePermissionPayload"],
	/** Deletes a single `Person` using a unique key. */
	deletePerson?: GraphQLTypes["DeletePersonPayload"],
	/** Deletes a single `PlatbyCategory` using a unique key. */
	deletePlatbyCategory?: GraphQLTypes["DeletePlatbyCategoryPayload"],
	/** Deletes a single `PlatbyCategoryGroup` using a unique key. */
	deletePlatbyCategoryGroup?: GraphQLTypes["DeletePlatbyCategoryGroupPayload"],
	/** Deletes a single `PlatbyGroup` using a unique key. */
	deletePlatbyGroup?: GraphQLTypes["DeletePlatbyGroupPayload"],
	/** Deletes a single `PlatbyGroupSkupina` using a unique key. */
	deletePlatbyGroupSkupina?: GraphQLTypes["DeletePlatbyGroupSkupinaPayload"],
	/** Deletes a single `PlatbyItem` using a unique key. */
	deletePlatbyItem?: GraphQLTypes["DeletePlatbyItemPayload"],
	/** Deletes a single `PlatbyRaw` using a unique key. */
	deletePlatbyRaw?: GraphQLTypes["DeletePlatbyRawPayload"],
	/** Deletes a single `Room` using a unique key. */
	deleteRoom?: GraphQLTypes["DeleteRoomPayload"],
	/** Deletes a single `RoomAttachment` using a unique key. */
	deleteRoomAttachment?: GraphQLTypes["DeleteRoomAttachmentPayload"],
	/** Deletes a single `Rozpi` using a unique key. */
	deleteRozpi?: GraphQLTypes["DeleteRozpiPayload"],
	/** Deletes a single `RozpisItem` using a unique key. */
	deleteRozpisItem?: GraphQLTypes["DeleteRozpisItemPayload"],
	/** Deletes a single `Skupiny` using a unique key. */
	deleteSkupiny?: GraphQLTypes["DeleteSkupinyPayload"],
	/** Deletes a single `Tenant` using a unique key. */
	deleteTenant?: GraphQLTypes["DeleteTenantPayload"],
	/** Deletes a single `TenantAttachment` using a unique key. */
	deleteTenantAttachment?: GraphQLTypes["DeleteTenantAttachmentPayload"],
	/** Deletes a single `TenantLocation` using a unique key. */
	deleteTenantLocation?: GraphQLTypes["DeleteTenantLocationPayload"],
	/** Deletes a single `TenantPerson` using a unique key. */
	deleteTenantPerson?: GraphQLTypes["DeleteTenantPersonPayload"],
	/** Deletes a single `Upozorneni` using a unique key. */
	deleteUpozorneni?: GraphQLTypes["DeleteUpozorneniPayload"],
	/** Deletes a single `UpozorneniSkupiny` using a unique key. */
	deleteUpozorneniSkupiny?: GraphQLTypes["DeleteUpozorneniSkupinyPayload"],
	/** Deletes a single `User` using a unique key. */
	deleteUser?: GraphQLTypes["DeleteUserPayload"],
	bookLesson?: GraphQLTypes["BookLessonPayload"],
	cancelLesson?: GraphQLTypes["CancelLessonPayload"],
	cancelParticipation?: GraphQLTypes["CancelParticipationPayload"],
	changePassword?: GraphQLTypes["ChangePasswordPayload"],
	confirmUser?: GraphQLTypes["ConfirmUserPayload"],
	createCouple?: GraphQLTypes["CreateCouplePayload"],
	createParticipation?: GraphQLTypes["CreateParticipationPayload"],
	createParticipationExternal?: GraphQLTypes["CreateParticipationExternalPayload"],
	fixUnpairedCouples?: GraphQLTypes["FixUnpairedCouplesPayload"],
	login?: GraphQLTypes["LoginPayload"],
	logout?: GraphQLTypes["LogoutPayload"],
	prospectFormDancer?: GraphQLTypes["ProspectFormDancerPayload"],
	reservationSetDesiredLessons?: GraphQLTypes["ReservationSetDesiredLessonsPayload"],
	resetPassword?: GraphQLTypes["ResetPasswordPayload"],
	submitForm?: GraphQLTypes["SubmitFormPayload"],
	verifyFunction?: GraphQLTypes["VerifyFunctionPayload"],
	uploadFile: GraphQLTypes["UploadFilePayload"],
	downloadFile: string
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
	aJmeno?: string,
	aKde?: string,
	aInfo?: string,
	aOd?: GraphQLTypes["Date"],
	aDo?: GraphQLTypes["Date"],
	aKapacita?: GraphQLTypes["BigInt"],
	aDokumenty?: string,
	aTimestamp?: GraphQLTypes["Datetime"],
	aLock?: boolean,
	aVisible?: boolean,
	summary?: string,
	isPublic?: boolean,
	enableNotes?: boolean
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
	aiIdRodic?: GraphQLTypes["BigInt"],
	aiUser?: GraphQLTypes["BigInt"],
	aiRokNarozeni?: number,
	notes?: string
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
	atKdo?: GraphQLTypes["BigInt"],
	atKat?: string,
	atJmeno: string,
	atText: string,
	atPreview: string,
	atFoto?: GraphQLTypes["BigInt"],
	atFotoMain?: GraphQLTypes["BigInt"],
	atTimestamp?: GraphQLTypes["Datetime"],
	atTimestampAdd?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Attachment` mutation. */
["CreateAttachmentPayload"]: {
	__typename: "CreateAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Attachment` that was created by this mutation. */
	attachment?: GraphQLTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?: GraphQLTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?: GraphQLTypes["AttachmentsEdge"]
};
	/** All input for the create `Attachment` mutation. */
["CreateAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Attachment` to be created by this mutation. */
	attachment: GraphQLTypes["AttachmentInput"]
};
	/** An input for mutations affecting `Attachment` */
["AttachmentInput"]: {
		objectName: string,
	previewObjectName?: string,
	uploadedBy?: GraphQLTypes["BigInt"],
	uploadedAt?: GraphQLTypes["Datetime"]
};
	/** The output of our create `AttendeeExternal` mutation. */
["CreateAttendeeExternalPayload"]: {
	__typename: "CreateAttendeeExternalPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeExternal` that was created by this mutation. */
	attendeeExternal?: GraphQLTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?: GraphQLTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?: GraphQLTypes["AttendeeExternalsEdge"]
};
	/** All input for the create `AttendeeExternal` mutation. */
["CreateAttendeeExternalInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `AttendeeExternal` to be created by this mutation. */
	attendeeExternal: GraphQLTypes["AttendeeExternalInput"]
};
	/** An input for mutations affecting `AttendeeExternal` */
["AttendeeExternalInput"]: {
		eventId: GraphQLTypes["BigInt"],
	firstName: string,
	lastName: string,
	email: string,
	phone: string,
	notes?: string,
	birthNumber?: string,
	guardianName?: string,
	managedBy?: GraphQLTypes["BigInt"],
	confirmedBy?: GraphQLTypes["BigInt"],
	confirmedAt?: GraphQLTypes["Datetime"],
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `AttendeeUser` mutation. */
["CreateAttendeeUserPayload"]: {
	__typename: "CreateAttendeeUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeUser` that was created by this mutation. */
	attendeeUser?: GraphQLTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?: GraphQLTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?: GraphQLTypes["AttendeeUsersEdge"]
};
	/** All input for the create `AttendeeUser` mutation. */
["CreateAttendeeUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `AttendeeUser` to be created by this mutation. */
	attendeeUser: GraphQLTypes["AttendeeUserInput"]
};
	/** An input for mutations affecting `AttendeeUser` */
["AttendeeUserInput"]: {
		id?: GraphQLTypes["BigInt"],
	eventId: GraphQLTypes["BigInt"],
	userId: GraphQLTypes["BigInt"],
	birthYear: number,
	notes?: string,
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `CohortGroup` mutation. */
["CreateCohortGroupPayload"]: {
	__typename: "CreateCohortGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `CohortGroup` that was created by this mutation. */
	cohortGroup?: GraphQLTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?: GraphQLTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?: GraphQLTypes["CohortGroupsEdge"]
};
	/** All input for the create `CohortGroup` mutation. */
["CreateCohortGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `CohortGroup` to be created by this mutation. */
	cohortGroup: GraphQLTypes["CohortGroupInput"]
};
	/** An input for mutations affecting `CohortGroup` */
["CohortGroupInput"]: {
		id?: GraphQLTypes["BigInt"],
	name: string,
	description?: string,
	ordering?: number,
	isPublic?: boolean,
	tenant?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	dTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Event` mutation. */
["CreateEventPayload"]: {
	__typename: "CreateEventPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Event` that was created by this mutation. */
	event?: GraphQLTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?: GraphQLTypes["EventsEdge"]
};
	/** All input for the create `Event` mutation. */
["CreateEventInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Event` to be created by this mutation. */
	event: GraphQLTypes["EventInput"]
};
	/** An input for mutations affecting `Event` */
["EventInput"]: {
		id?: GraphQLTypes["BigInt"],
	name: string,
	locationText: string,
	description: string,
	since: GraphQLTypes["Date"],
	until: GraphQLTypes["Date"],
	capacity?: GraphQLTypes["BigInt"],
	filesLegacy?: string,
	updatedAt?: GraphQLTypes["Datetime"],
	isLocked?: boolean,
	isVisible?: boolean,
	summary?: string,
	isPublic?: boolean,
	enableNotes?: boolean,
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `FormResponse` mutation. */
["CreateFormResponsePayload"]: {
	__typename: "CreateFormResponsePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `FormResponse` that was created by this mutation. */
	formResponse?: GraphQLTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?: GraphQLTypes["FormResponsesEdge"]
};
	/** All input for the create `FormResponse` mutation. */
["CreateFormResponseInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `FormResponse` to be created by this mutation. */
	formResponse: GraphQLTypes["FormResponseInput"]
};
	/** An input for mutations affecting `FormResponse` */
["FormResponseInput"]: {
		id?: GraphQLTypes["BigInt"],
	type: string,
	data: GraphQLTypes["JSON"],
	url: string,
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	tenantId?: GraphQLTypes["BigInt"]
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
	gdHidden?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	gfTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Location` mutation. */
["CreateLocationPayload"]: {
	__typename: "CreateLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Location` that was created by this mutation. */
	location?: GraphQLTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?: GraphQLTypes["LocationsEdge"]
};
	/** All input for the create `Location` mutation. */
["CreateLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Location` to be created by this mutation. */
	location: GraphQLTypes["LocationInput"]
};
	/** An input for mutations affecting `Location` */
["LocationInput"]: {
		id?: GraphQLTypes["BigInt"],
	name: string,
	description: GraphQLTypes["JSON"]
};
	/** The output of our create `LocationAttachment` mutation. */
["CreateLocationAttachmentPayload"]: {
	__typename: "CreateLocationAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `LocationAttachment` that was created by this mutation. */
	locationAttachment?: GraphQLTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?: GraphQLTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?: GraphQLTypes["LocationAttachmentsEdge"]
};
	/** All input for the create `LocationAttachment` mutation. */
["CreateLocationAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `LocationAttachment` to be created by this mutation. */
	locationAttachment: GraphQLTypes["LocationAttachmentInput"]
};
	/** An input for mutations affecting `LocationAttachment` */
["LocationAttachmentInput"]: {
		locationId: GraphQLTypes["BigInt"],
	objectName: string
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
	nMaxPocetHod?: number,
	nOd: GraphQLTypes["Date"],
	nDo: GraphQLTypes["Date"],
	nVisible?: boolean,
	nLock?: boolean,
	nTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	niLock?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?: GraphQLTypes["User"],
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
	pTimestampArchive?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"]
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
	pnPartnerka: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"]
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
	peMain: number,
	id?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Person` mutation. */
["CreatePersonPayload"]: {
	__typename: "CreatePersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Person` that was created by this mutation. */
	person?: GraphQLTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?: GraphQLTypes["PeopleEdge"]
};
	/** All input for the create `Person` mutation. */
["CreatePersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Person` to be created by this mutation. */
	person: GraphQLTypes["PersonInput"]
};
	/** An input for mutations affecting `Person` */
["PersonInput"]: {
		id?: GraphQLTypes["BigInt"],
	firstName: string,
	lastName: string,
	gender: GraphQLTypes["GenderType"]
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
	pcVisible?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	pcgIdCategory: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	pgBase?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	pgsIdGroup: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	piPrefix?: number,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	prDiscarded?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Room` mutation. */
["CreateRoomPayload"]: {
	__typename: "CreateRoomPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Room` that was created by this mutation. */
	room?: GraphQLTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?: GraphQLTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?: GraphQLTypes["RoomsEdge"]
};
	/** All input for the create `Room` mutation. */
["CreateRoomInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Room` to be created by this mutation. */
	room: GraphQLTypes["RoomInput"]
};
	/** An input for mutations affecting `Room` */
["RoomInput"]: {
		id?: GraphQLTypes["BigInt"],
	name: string,
	description: GraphQLTypes["JSON"],
	location?: GraphQLTypes["BigInt"]
};
	/** The output of our create `RoomAttachment` mutation. */
["CreateRoomAttachmentPayload"]: {
	__typename: "CreateRoomAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RoomAttachment` that was created by this mutation. */
	roomAttachment?: GraphQLTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?: GraphQLTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?: GraphQLTypes["RoomAttachmentsEdge"]
};
	/** All input for the create `RoomAttachment` mutation. */
["CreateRoomAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `RoomAttachment` to be created by this mutation. */
	roomAttachment: GraphQLTypes["RoomAttachmentInput"]
};
	/** An input for mutations affecting `RoomAttachment` */
["RoomAttachmentInput"]: {
		roomId: GraphQLTypes["BigInt"],
	objectName: string
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
	rTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	riLock?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?: GraphQLTypes["CohortGroup"],
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
	sColorText?: string,
	sLocation?: string,
	sVisible?: boolean,
	ordering?: number,
	internalInfo?: string,
	cohortGroup?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our create `Tenant` mutation. */
["CreateTenantPayload"]: {
	__typename: "CreateTenantPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Tenant` that was created by this mutation. */
	tenant?: GraphQLTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?: GraphQLTypes["TenantsEdge"]
};
	/** All input for the create `Tenant` mutation. */
["CreateTenantInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `Tenant` to be created by this mutation. */
	tenant: GraphQLTypes["TenantInput"]
};
	/** An input for mutations affecting `Tenant` */
["TenantInput"]: {
		id?: GraphQLTypes["BigInt"],
	name: string,
	memberInfo: string,
	origins?: Array<string | undefined>
};
	/** The output of our create `TenantAttachment` mutation. */
["CreateTenantAttachmentPayload"]: {
	__typename: "CreateTenantAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantAttachment` that was created by this mutation. */
	tenantAttachment?: GraphQLTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?: GraphQLTypes["TenantAttachmentsEdge"]
};
	/** All input for the create `TenantAttachment` mutation. */
["CreateTenantAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `TenantAttachment` to be created by this mutation. */
	tenantAttachment: GraphQLTypes["TenantAttachmentInput"]
};
	/** An input for mutations affecting `TenantAttachment` */
["TenantAttachmentInput"]: {
		tenantId: GraphQLTypes["BigInt"],
	objectName: string,
	type?: GraphQLTypes["TenantAttachmentType"]
};
	/** The output of our create `TenantLocation` mutation. */
["CreateTenantLocationPayload"]: {
	__typename: "CreateTenantLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantLocation` that was created by this mutation. */
	tenantLocation?: GraphQLTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?: GraphQLTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?: GraphQLTypes["TenantLocationsEdge"]
};
	/** All input for the create `TenantLocation` mutation. */
["CreateTenantLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `TenantLocation` to be created by this mutation. */
	tenantLocation: GraphQLTypes["TenantLocationInput"]
};
	/** An input for mutations affecting `TenantLocation` */
["TenantLocationInput"]: {
		tenantId: GraphQLTypes["BigInt"],
	locationId: GraphQLTypes["BigInt"]
};
	/** The output of our create `TenantPerson` mutation. */
["CreateTenantPersonPayload"]: {
	__typename: "CreateTenantPersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantPerson` that was created by this mutation. */
	tenantPerson?: GraphQLTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?: GraphQLTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?: GraphQLTypes["TenantPeopleEdge"]
};
	/** All input for the create `TenantPerson` mutation. */
["CreateTenantPersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** The `TenantPerson` to be created by this mutation. */
	tenantPerson: GraphQLTypes["TenantPersonInput"]
};
	/** An input for mutations affecting `TenantPerson` */
["TenantPersonInput"]: {
		tenantId: GraphQLTypes["BigInt"],
	personId: GraphQLTypes["BigInt"]
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
	upKdo?: GraphQLTypes["BigInt"],
	upNadpis: string,
	upText: string,
	upBarvy?: GraphQLTypes["BigInt"],
	upLock?: boolean,
	upTimestamp?: GraphQLTypes["Datetime"],
	upTimestampAdd?: GraphQLTypes["Datetime"],
	scheduledSince?: GraphQLTypes["Datetime"],
	scheduledUntil?: GraphQLTypes["Datetime"],
	isVisible?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	upsPopis: string,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	uGroup?: GraphQLTypes["BigInt"],
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
	uGdprSignedAt?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateAktuality` mutation. */
["UpdateAktualityInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Aktuality` being updated. */
	patch: GraphQLTypes["AktualityPatch"],
	atId: GraphQLTypes["BigInt"]
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
	atTimestampAdd?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Attachment` mutation. */
["UpdateAttachmentPayload"]: {
	__typename: "UpdateAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Attachment` that was updated by this mutation. */
	attachment?: GraphQLTypes["Attachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?: GraphQLTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?: GraphQLTypes["AttachmentsEdge"]
};
	/** All input for the `updateAttachment` mutation. */
["UpdateAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Attachment` being updated. */
	patch: GraphQLTypes["AttachmentPatch"],
	objectName: string
};
	/** Represents an update to a `Attachment`. Fields that are set will be updated. */
["AttachmentPatch"]: {
		objectName?: string,
	previewObjectName?: string,
	uploadedBy?: GraphQLTypes["BigInt"],
	uploadedAt?: GraphQLTypes["Datetime"]
};
	/** The output of our update `AttendeeExternal` mutation. */
["UpdateAttendeeExternalPayload"]: {
	__typename: "UpdateAttendeeExternalPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeExternal` that was updated by this mutation. */
	attendeeExternal?: GraphQLTypes["AttendeeExternal"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?: GraphQLTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?: GraphQLTypes["AttendeeExternalsEdge"]
};
	/** All input for the `updateAttendeeExternal` mutation. */
["UpdateAttendeeExternalInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `AttendeeExternal` being updated. */
	patch: GraphQLTypes["AttendeeExternalPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `AttendeeExternal`. Fields that are set will be updated. */
["AttendeeExternalPatch"]: {
		eventId?: GraphQLTypes["BigInt"],
	firstName?: string,
	lastName?: string,
	email?: string,
	phone?: string,
	notes?: string,
	birthNumber?: string,
	guardianName?: string,
	managedBy?: GraphQLTypes["BigInt"],
	confirmedBy?: GraphQLTypes["BigInt"],
	confirmedAt?: GraphQLTypes["Datetime"],
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `AttendeeUser` mutation. */
["UpdateAttendeeUserPayload"]: {
	__typename: "UpdateAttendeeUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeUser` that was updated by this mutation. */
	attendeeUser?: GraphQLTypes["AttendeeUser"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?: GraphQLTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?: GraphQLTypes["AttendeeUsersEdge"]
};
	/** All input for the `updateAttendeeUser` mutation. */
["UpdateAttendeeUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `AttendeeUser` being updated. */
	patch: GraphQLTypes["AttendeeUserPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `AttendeeUser`. Fields that are set will be updated. */
["AttendeeUserPatch"]: {
		id?: GraphQLTypes["BigInt"],
	eventId?: GraphQLTypes["BigInt"],
	userId?: GraphQLTypes["BigInt"],
	birthYear?: number,
	notes?: string,
	tenantId?: GraphQLTypes["BigInt"]
};
	/** All input for the `updateAttendeeUserByUserIdAndEventId` mutation. */
["UpdateAttendeeUserByUserIdAndEventIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `AttendeeUser` being updated. */
	patch: GraphQLTypes["AttendeeUserPatch"],
	userId: GraphQLTypes["BigInt"],
	eventId: GraphQLTypes["BigInt"]
};
	/** The output of our update `CohortGroup` mutation. */
["UpdateCohortGroupPayload"]: {
	__typename: "UpdateCohortGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `CohortGroup` that was updated by this mutation. */
	cohortGroup?: GraphQLTypes["CohortGroup"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?: GraphQLTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?: GraphQLTypes["CohortGroupsEdge"]
};
	/** All input for the `updateCohortGroup` mutation. */
["UpdateCohortGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `CohortGroup` being updated. */
	patch: GraphQLTypes["CohortGroupPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `CohortGroup`. Fields that are set will be updated. */
["CohortGroupPatch"]: {
		id?: GraphQLTypes["BigInt"],
	name?: string,
	description?: string,
	ordering?: number,
	isPublic?: boolean,
	tenant?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateDokumenty` mutation. */
["UpdateDokumentyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Dokumenty` being updated. */
	patch: GraphQLTypes["DokumentyPatch"],
	dId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
["DokumentyPatch"]: {
		dId?: GraphQLTypes["BigInt"],
	dPath?: string,
	dName?: string,
	dFilename?: string,
	dKategorie?: number,
	dKdo?: GraphQLTypes["BigInt"],
	dTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Event` mutation. */
["UpdateEventPayload"]: {
	__typename: "UpdateEventPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Event` that was updated by this mutation. */
	event?: GraphQLTypes["Event"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?: GraphQLTypes["EventsEdge"]
};
	/** All input for the `updateEvent` mutation. */
["UpdateEventInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Event` being updated. */
	patch: GraphQLTypes["EventPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Event`. Fields that are set will be updated. */
["EventPatch"]: {
		id?: GraphQLTypes["BigInt"],
	name?: string,
	locationText?: string,
	description?: string,
	since?: GraphQLTypes["Date"],
	until?: GraphQLTypes["Date"],
	capacity?: GraphQLTypes["BigInt"],
	filesLegacy?: string,
	updatedAt?: GraphQLTypes["Datetime"],
	isLocked?: boolean,
	isVisible?: boolean,
	summary?: string,
	isPublic?: boolean,
	enableNotes?: boolean,
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `FormResponse` mutation. */
["UpdateFormResponsePayload"]: {
	__typename: "UpdateFormResponsePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `FormResponse` that was updated by this mutation. */
	formResponse?: GraphQLTypes["FormResponse"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?: GraphQLTypes["FormResponsesEdge"]
};
	/** All input for the `updateFormResponse` mutation. */
["UpdateFormResponseInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `FormResponse` being updated. */
	patch: GraphQLTypes["FormResponsePatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `FormResponse`. Fields that are set will be updated. */
["FormResponsePatch"]: {
		id?: GraphQLTypes["BigInt"],
	type?: string,
	data?: GraphQLTypes["JSON"],
	url?: string,
	createdAt?: GraphQLTypes["Datetime"],
	updatedAt?: GraphQLTypes["Datetime"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateGalerieDir` mutation. */
["UpdateGalerieDirInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `GalerieDir` being updated. */
	patch: GraphQLTypes["GalerieDirPatch"],
	gdId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
["GalerieDirPatch"]: {
		gdId?: GraphQLTypes["BigInt"],
	gdIdRodic?: GraphQLTypes["BigInt"],
	gdName?: string,
	gdLevel?: number,
	gdPath?: string,
	gdHidden?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateGalerieFoto` mutation. */
["UpdateGalerieFotoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `GalerieFoto` being updated. */
	patch: GraphQLTypes["GalerieFotoPatch"],
	gfId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
["GalerieFotoPatch"]: {
		gfId?: GraphQLTypes["BigInt"],
	gfIdRodic?: GraphQLTypes["BigInt"],
	gfName?: string,
	gfPath?: string,
	gfKdo?: GraphQLTypes["BigInt"],
	gfTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Location` mutation. */
["UpdateLocationPayload"]: {
	__typename: "UpdateLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Location` that was updated by this mutation. */
	location?: GraphQLTypes["Location"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?: GraphQLTypes["LocationsEdge"]
};
	/** All input for the `updateLocation` mutation. */
["UpdateLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Location` being updated. */
	patch: GraphQLTypes["LocationPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Location`. Fields that are set will be updated. */
["LocationPatch"]: {
		id?: GraphQLTypes["BigInt"],
	name?: string,
	description?: GraphQLTypes["JSON"]
};
	/** The output of our update `LocationAttachment` mutation. */
["UpdateLocationAttachmentPayload"]: {
	__typename: "UpdateLocationAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `LocationAttachment` that was updated by this mutation. */
	locationAttachment?: GraphQLTypes["LocationAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?: GraphQLTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?: GraphQLTypes["LocationAttachmentsEdge"]
};
	/** All input for the `updateLocationAttachment` mutation. */
["UpdateLocationAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `LocationAttachment` being updated. */
	patch: GraphQLTypes["LocationAttachmentPatch"],
	locationId: GraphQLTypes["BigInt"],
	objectName: string
};
	/** Represents an update to a `LocationAttachment`. Fields that are set will be updated. */
["LocationAttachmentPatch"]: {
		locationId?: GraphQLTypes["BigInt"],
	objectName?: string
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
	/** All input for the `updateNabidka` mutation. */
["UpdateNabidkaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Nabidka` being updated. */
	patch: GraphQLTypes["NabidkaPatch"],
	nId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
["NabidkaPatch"]: {
		nId?: GraphQLTypes["BigInt"],
	nTrener?: GraphQLTypes["BigInt"],
	nPocetHod?: number,
	nMaxPocetHod?: number,
	nOd?: GraphQLTypes["Date"],
	nDo?: GraphQLTypes["Date"],
	nVisible?: boolean,
	nLock?: boolean,
	nTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateNabidkaItem` mutation. */
["UpdateNabidkaItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch: GraphQLTypes["NabidkaItemPatch"],
	niId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
["NabidkaItemPatch"]: {
		niId?: GraphQLTypes["BigInt"],
	niIdRodic?: GraphQLTypes["BigInt"],
	niPartner?: GraphQLTypes["BigInt"],
	niPocetHod?: number,
	niLock?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** All input for the `updateNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["UpdateNabidkaItemByNiPartnerAndNiIdRodicInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `NabidkaItem` being updated. */
	patch: GraphQLTypes["NabidkaItemPatch"],
	niPartner: GraphQLTypes["BigInt"],
	niIdRodic: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePage` mutation. */
["UpdatePageInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Page` being updated. */
	patch: GraphQLTypes["PagePatch"],
	id: number
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
	/** All input for the `updateParameter` mutation. */
["UpdateParameterInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Parameter` being updated. */
	patch: GraphQLTypes["ParameterPatch"],
	paName: string
};
	/** Represents an update to a `Parameter`. Fields that are set will be updated. */
["ParameterPatch"]: {
		paName?: string,
	paValue?: string
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?: GraphQLTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?: GraphQLTypes["PariesEdge"]
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
	pTimestampArchive?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateParyNavrh` mutation. */
["UpdateParyNavrhInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `ParyNavrh` being updated. */
	patch: GraphQLTypes["ParyNavrhPatch"],
	pnId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
["ParyNavrhPatch"]: {
		pnId?: GraphQLTypes["BigInt"],
	pnNavrhl?: GraphQLTypes["BigInt"],
	pnPartner?: GraphQLTypes["BigInt"],
	pnPartnerka?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePermission` mutation. */
["UpdatePermissionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Permission` being updated. */
	patch: GraphQLTypes["PermissionPatch"],
	peId: GraphQLTypes["BigInt"]
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
	peMain?: number,
	id?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Person` mutation. */
["UpdatePersonPayload"]: {
	__typename: "UpdatePersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Person` that was updated by this mutation. */
	person?: GraphQLTypes["Person"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?: GraphQLTypes["PeopleEdge"]
};
	/** All input for the `updatePerson` mutation. */
["UpdatePersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Person` being updated. */
	patch: GraphQLTypes["PersonPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Person`. Fields that are set will be updated. */
["PersonPatch"]: {
		id?: GraphQLTypes["BigInt"],
	firstName?: string,
	lastName?: string,
	gender?: GraphQLTypes["GenderType"]
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
	/** All input for the `updatePlatbyCategory` mutation. */
["UpdatePlatbyCategoryInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
	patch: GraphQLTypes["PlatbyCategoryPatch"],
	pcId: GraphQLTypes["BigInt"]
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
	pcVisible?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePlatbyCategoryGroup` mutation. */
["UpdatePlatbyCategoryGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
	patch: GraphQLTypes["PlatbyCategoryGroupPatch"],
	pcgId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
["PlatbyCategoryGroupPatch"]: {
		pcgId?: GraphQLTypes["BigInt"],
	pcgIdGroup?: GraphQLTypes["BigInt"],
	pcgIdCategory?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePlatbyGroup` mutation. */
["UpdatePlatbyGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
	patch: GraphQLTypes["PlatbyGroupPatch"],
	pgId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
["PlatbyGroupPatch"]: {
		pgId?: GraphQLTypes["BigInt"],
	pgType?: GraphQLTypes["BigFloat"],
	pgName?: string,
	pgDescription?: string,
	pgBase?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePlatbyGroupSkupina` mutation. */
["UpdatePlatbyGroupSkupinaInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
	patch: GraphQLTypes["PlatbyGroupSkupinaPatch"],
	pgsId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
["PlatbyGroupSkupinaPatch"]: {
		pgsId?: GraphQLTypes["BigInt"],
	pgsIdSkupina?: GraphQLTypes["BigInt"],
	pgsIdGroup?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePlatbyItem` mutation. */
["UpdatePlatbyItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyItem` being updated. */
	patch: GraphQLTypes["PlatbyItemPatch"],
	piId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
["PlatbyItemPatch"]: {
		piId?: GraphQLTypes["BigInt"],
	piIdUser?: GraphQLTypes["BigInt"],
	piIdCategory?: GraphQLTypes["BigInt"],
	piIdRaw?: GraphQLTypes["BigInt"],
	piAmount?: GraphQLTypes["BigFloat"],
	piDate?: GraphQLTypes["Date"],
	piPrefix?: number,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updatePlatbyRaw` mutation. */
["UpdatePlatbyRawInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
	patch: GraphQLTypes["PlatbyRawPatch"],
	prId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
["PlatbyRawPatch"]: {
		prId?: GraphQLTypes["BigInt"],
	prRaw?: string,
	prHash?: string,
	prSorted?: boolean,
	prDiscarded?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Room` mutation. */
["UpdateRoomPayload"]: {
	__typename: "UpdateRoomPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Room` that was updated by this mutation. */
	room?: GraphQLTypes["Room"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?: GraphQLTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?: GraphQLTypes["RoomsEdge"]
};
	/** All input for the `updateRoom` mutation. */
["UpdateRoomInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Room` being updated. */
	patch: GraphQLTypes["RoomPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Room`. Fields that are set will be updated. */
["RoomPatch"]: {
		id?: GraphQLTypes["BigInt"],
	name?: string,
	description?: GraphQLTypes["JSON"],
	location?: GraphQLTypes["BigInt"]
};
	/** The output of our update `RoomAttachment` mutation. */
["UpdateRoomAttachmentPayload"]: {
	__typename: "UpdateRoomAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RoomAttachment` that was updated by this mutation. */
	roomAttachment?: GraphQLTypes["RoomAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?: GraphQLTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?: GraphQLTypes["RoomAttachmentsEdge"]
};
	/** All input for the `updateRoomAttachment` mutation. */
["UpdateRoomAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `RoomAttachment` being updated. */
	patch: GraphQLTypes["RoomAttachmentPatch"],
	roomId: GraphQLTypes["BigInt"],
	objectName: string
};
	/** Represents an update to a `RoomAttachment`. Fields that are set will be updated. */
["RoomAttachmentPatch"]: {
		roomId?: GraphQLTypes["BigInt"],
	objectName?: string
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
	/** All input for the `updateRozpi` mutation. */
["UpdateRozpiInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Rozpi` being updated. */
	patch: GraphQLTypes["RozpiPatch"],
	rId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
["RozpiPatch"]: {
		rId?: GraphQLTypes["BigInt"],
	rTrener?: GraphQLTypes["BigInt"],
	rKde?: string,
	rDatum?: GraphQLTypes["Date"],
	rVisible?: boolean,
	rLock?: boolean,
	rTimestamp?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateRozpisItem` mutation. */
["UpdateRozpisItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `RozpisItem` being updated. */
	patch: GraphQLTypes["RozpisItemPatch"],
	riId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
["RozpisItemPatch"]: {
		riId?: GraphQLTypes["BigInt"],
	riIdRodic?: GraphQLTypes["BigInt"],
	riPartner?: GraphQLTypes["BigInt"],
	riOd?: GraphQLTypes["Time"],
	riDo?: GraphQLTypes["Time"],
	riLock?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?: GraphQLTypes["CohortGroup"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?: GraphQLTypes["SkupiniesEdge"]
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
	/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
["SkupinyPatch"]: {
		sId?: GraphQLTypes["BigInt"],
	sName?: string,
	sDescription?: string,
	sColorRgb?: string,
	sColorText?: string,
	sLocation?: string,
	sVisible?: boolean,
	ordering?: number,
	internalInfo?: string,
	cohortGroup?: GraphQLTypes["BigInt"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `Tenant` mutation. */
["UpdateTenantPayload"]: {
	__typename: "UpdateTenantPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Tenant` that was updated by this mutation. */
	tenant?: GraphQLTypes["Tenant"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?: GraphQLTypes["TenantsEdge"]
};
	/** All input for the `updateTenant` mutation. */
["UpdateTenantInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Tenant` being updated. */
	patch: GraphQLTypes["TenantPatch"],
	id: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `Tenant`. Fields that are set will be updated. */
["TenantPatch"]: {
		id?: GraphQLTypes["BigInt"],
	name?: string,
	memberInfo?: string,
	origins?: Array<string | undefined>
};
	/** The output of our update `TenantAttachment` mutation. */
["UpdateTenantAttachmentPayload"]: {
	__typename: "UpdateTenantAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantAttachment` that was updated by this mutation. */
	tenantAttachment?: GraphQLTypes["TenantAttachment"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?: GraphQLTypes["TenantAttachmentsEdge"]
};
	/** All input for the `updateTenantAttachment` mutation. */
["UpdateTenantAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `TenantAttachment` being updated. */
	patch: GraphQLTypes["TenantAttachmentPatch"],
	tenantId: GraphQLTypes["BigInt"],
	objectName: string
};
	/** Represents an update to a `TenantAttachment`. Fields that are set will be updated. */
["TenantAttachmentPatch"]: {
		tenantId?: GraphQLTypes["BigInt"],
	objectName?: string,
	type?: GraphQLTypes["TenantAttachmentType"]
};
	/** The output of our update `TenantLocation` mutation. */
["UpdateTenantLocationPayload"]: {
	__typename: "UpdateTenantLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantLocation` that was updated by this mutation. */
	tenantLocation?: GraphQLTypes["TenantLocation"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?: GraphQLTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?: GraphQLTypes["TenantLocationsEdge"]
};
	/** All input for the `updateTenantLocation` mutation. */
["UpdateTenantLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `TenantLocation` being updated. */
	patch: GraphQLTypes["TenantLocationPatch"],
	tenantId: GraphQLTypes["BigInt"],
	locationId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `TenantLocation`. Fields that are set will be updated. */
["TenantLocationPatch"]: {
		tenantId?: GraphQLTypes["BigInt"],
	locationId?: GraphQLTypes["BigInt"]
};
	/** The output of our update `TenantPerson` mutation. */
["UpdateTenantPersonPayload"]: {
	__typename: "UpdateTenantPersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantPerson` that was updated by this mutation. */
	tenantPerson?: GraphQLTypes["TenantPerson"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?: GraphQLTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?: GraphQLTypes["TenantPeopleEdge"]
};
	/** All input for the `updateTenantPerson` mutation. */
["UpdateTenantPersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `TenantPerson` being updated. */
	patch: GraphQLTypes["TenantPersonPatch"],
	tenantId: GraphQLTypes["BigInt"],
	personId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `TenantPerson`. Fields that are set will be updated. */
["TenantPersonPatch"]: {
		tenantId?: GraphQLTypes["BigInt"],
	personId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateUpozorneni` mutation. */
["UpdateUpozorneniInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `Upozorneni` being updated. */
	patch: GraphQLTypes["UpozorneniPatch"],
	upId: GraphQLTypes["BigInt"]
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
	upTimestampAdd?: GraphQLTypes["Datetime"],
	scheduledSince?: GraphQLTypes["Datetime"],
	scheduledUntil?: GraphQLTypes["Datetime"],
	isVisible?: boolean,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateUpozorneniSkupiny` mutation. */
["UpdateUpozorneniSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
	patch: GraphQLTypes["UpozorneniSkupinyPatch"],
	upsId: GraphQLTypes["BigInt"]
};
	/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
["UpozorneniSkupinyPatch"]: {
		upsId?: GraphQLTypes["BigInt"],
	upsIdRodic?: GraphQLTypes["BigInt"],
	upsIdSkupina?: GraphQLTypes["BigInt"],
	upsColor?: string,
	upsPopis?: string,
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `updateUser` mutation. */
["UpdateUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	/** An object where the defined keys will be set on the `User` being updated. */
	patch: GraphQLTypes["UserPatch"],
	uId: GraphQLTypes["BigInt"]
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
	uGdprSignedAt?: GraphQLTypes["Datetime"],
	id?: GraphQLTypes["BigInt"],
	tenantId?: GraphQLTypes["BigInt"]
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
	/** All input for the `deleteAktuality` mutation. */
["DeleteAktualityInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	atId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Attachment` mutation. */
["DeleteAttachmentPayload"]: {
	__typename: "DeleteAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Attachment` that was deleted by this mutation. */
	attachment?: GraphQLTypes["Attachment"],
	deletedAttachmentNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Attachment`. */
	userByUploadedBy?: GraphQLTypes["User"],
	/** An edge for our `Attachment`. May be used by Relay 1. */
	attachmentEdge?: GraphQLTypes["AttachmentsEdge"]
};
	/** All input for the `deleteAttachment` mutation. */
["DeleteAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	objectName: string
};
	/** The output of our delete `AttendeeExternal` mutation. */
["DeleteAttendeeExternalPayload"]: {
	__typename: "DeleteAttendeeExternalPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeExternal` that was deleted by this mutation. */
	attendeeExternal?: GraphQLTypes["AttendeeExternal"],
	deletedAttendeeExternalNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeExternal`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByManagedBy?: GraphQLTypes["User"],
	/** Reads a single `User` that is related to this `AttendeeExternal`. */
	userByConfirmedBy?: GraphQLTypes["User"],
	/** An edge for our `AttendeeExternal`. May be used by Relay 1. */
	attendeeExternalEdge?: GraphQLTypes["AttendeeExternalsEdge"]
};
	/** All input for the `deleteAttendeeExternal` mutation. */
["DeleteAttendeeExternalInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** The output of our delete `AttendeeUser` mutation. */
["DeleteAttendeeUserPayload"]: {
	__typename: "DeleteAttendeeUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `AttendeeUser` that was deleted by this mutation. */
	attendeeUser?: GraphQLTypes["AttendeeUser"],
	deletedAttendeeUserNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Event` that is related to this `AttendeeUser`. */
	event?: GraphQLTypes["Event"],
	/** Reads a single `User` that is related to this `AttendeeUser`. */
	user?: GraphQLTypes["User"],
	/** An edge for our `AttendeeUser`. May be used by Relay 1. */
	attendeeUserEdge?: GraphQLTypes["AttendeeUsersEdge"]
};
	/** All input for the `deleteAttendeeUser` mutation. */
["DeleteAttendeeUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** All input for the `deleteAttendeeUserByUserIdAndEventId` mutation. */
["DeleteAttendeeUserByUserIdAndEventIdInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	userId: GraphQLTypes["BigInt"],
	eventId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `CohortGroup` mutation. */
["DeleteCohortGroupPayload"]: {
	__typename: "DeleteCohortGroupPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `CohortGroup` that was deleted by this mutation. */
	cohortGroup?: GraphQLTypes["CohortGroup"],
	deletedCohortGroupNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `CohortGroup`. */
	tenantByTenant?: GraphQLTypes["Tenant"],
	/** An edge for our `CohortGroup`. May be used by Relay 1. */
	cohortGroupEdge?: GraphQLTypes["CohortGroupsEdge"]
};
	/** All input for the `deleteCohortGroup` mutation. */
["DeleteCohortGroupInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
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
	/** All input for the `deleteDokumenty` mutation. */
["DeleteDokumentyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	dId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Event` mutation. */
["DeleteEventPayload"]: {
	__typename: "DeleteEventPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Event` that was deleted by this mutation. */
	event?: GraphQLTypes["Event"],
	deletedEventNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Event`. May be used by Relay 1. */
	eventEdge?: GraphQLTypes["EventsEdge"]
};
	/** All input for the `deleteEvent` mutation. */
["DeleteEventInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** The output of our delete `FormResponse` mutation. */
["DeleteFormResponsePayload"]: {
	__typename: "DeleteFormResponsePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `FormResponse` that was deleted by this mutation. */
	formResponse?: GraphQLTypes["FormResponse"],
	deletedFormResponseNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `FormResponse`. May be used by Relay 1. */
	formResponseEdge?: GraphQLTypes["FormResponsesEdge"]
};
	/** All input for the `deleteFormResponse` mutation. */
["DeleteFormResponseInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
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
	/** All input for the `deleteGalerieFoto` mutation. */
["DeleteGalerieFotoInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	gfId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Location` mutation. */
["DeleteLocationPayload"]: {
	__typename: "DeleteLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Location` that was deleted by this mutation. */
	location?: GraphQLTypes["Location"],
	deletedLocationNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Location`. May be used by Relay 1. */
	locationEdge?: GraphQLTypes["LocationsEdge"]
};
	/** All input for the `deleteLocation` mutation. */
["DeleteLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** The output of our delete `LocationAttachment` mutation. */
["DeleteLocationAttachmentPayload"]: {
	__typename: "DeleteLocationAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `LocationAttachment` that was deleted by this mutation. */
	locationAttachment?: GraphQLTypes["LocationAttachment"],
	deletedLocationAttachmentNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `LocationAttachment`. */
	location?: GraphQLTypes["Location"],
	/** Reads a single `Attachment` that is related to this `LocationAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `LocationAttachment`. May be used by Relay 1. */
	locationAttachmentEdge?: GraphQLTypes["LocationAttachmentsEdge"]
};
	/** All input for the `deleteLocationAttachment` mutation. */
["DeleteLocationAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	locationId: GraphQLTypes["BigInt"],
	objectName: string
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
	/** All input for the `deleteNabidkaItem` mutation. */
["DeleteNabidkaItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	niId: GraphQLTypes["BigInt"]
};
	/** All input for the `deleteNabidkaItemByNiPartnerAndNiIdRodic` mutation. */
["DeleteNabidkaItemByNiPartnerAndNiIdRodicInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	niPartner: GraphQLTypes["BigInt"],
	niIdRodic: GraphQLTypes["BigInt"]
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
	/** Reads a single `User` that is related to this `Pary`. */
	userByPIdPartnerka?: GraphQLTypes["User"],
	/** An edge for our `Pary`. May be used by Relay 1. */
	paryEdge?: GraphQLTypes["PariesEdge"]
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
	/** All input for the `deletePermission` mutation. */
["DeletePermissionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	peId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Person` mutation. */
["DeletePersonPayload"]: {
	__typename: "DeletePersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Person` that was deleted by this mutation. */
	person?: GraphQLTypes["Person"],
	deletedPersonNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Person`. May be used by Relay 1. */
	personEdge?: GraphQLTypes["PeopleEdge"]
};
	/** All input for the `deletePerson` mutation. */
["DeletePersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
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
	/** All input for the `deletePlatbyRaw` mutation. */
["DeletePlatbyRawInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	prId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Room` mutation. */
["DeleteRoomPayload"]: {
	__typename: "DeleteRoomPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Room` that was deleted by this mutation. */
	room?: GraphQLTypes["Room"],
	deletedRoomNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Location` that is related to this `Room`. */
	locationByLocation?: GraphQLTypes["Location"],
	/** An edge for our `Room`. May be used by Relay 1. */
	roomEdge?: GraphQLTypes["RoomsEdge"]
};
	/** All input for the `deleteRoom` mutation. */
["DeleteRoomInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** The output of our delete `RoomAttachment` mutation. */
["DeleteRoomAttachmentPayload"]: {
	__typename: "DeleteRoomAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `RoomAttachment` that was deleted by this mutation. */
	roomAttachment?: GraphQLTypes["RoomAttachment"],
	deletedRoomAttachmentNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Room` that is related to this `RoomAttachment`. */
	room?: GraphQLTypes["Room"],
	/** Reads a single `Attachment` that is related to this `RoomAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `RoomAttachment`. May be used by Relay 1. */
	roomAttachmentEdge?: GraphQLTypes["RoomAttachmentsEdge"]
};
	/** All input for the `deleteRoomAttachment` mutation. */
["DeleteRoomAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	roomId: GraphQLTypes["BigInt"],
	objectName: string
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
	/** All input for the `deleteRozpisItem` mutation. */
["DeleteRozpisItemInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	riId: GraphQLTypes["BigInt"]
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
	/** Reads a single `CohortGroup` that is related to this `Skupiny`. */
	cohortGroupByCohortGroup?: GraphQLTypes["CohortGroup"],
	/** An edge for our `Skupiny`. May be used by Relay 1. */
	skupinyEdge?: GraphQLTypes["SkupiniesEdge"]
};
	/** All input for the `deleteSkupiny` mutation. */
["DeleteSkupinyInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	sId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `Tenant` mutation. */
["DeleteTenantPayload"]: {
	__typename: "DeleteTenantPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `Tenant` that was deleted by this mutation. */
	tenant?: GraphQLTypes["Tenant"],
	deletedTenantNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** An edge for our `Tenant`. May be used by Relay 1. */
	tenantEdge?: GraphQLTypes["TenantsEdge"]
};
	/** All input for the `deleteTenant` mutation. */
["DeleteTenantInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"]
};
	/** The output of our delete `TenantAttachment` mutation. */
["DeleteTenantAttachmentPayload"]: {
	__typename: "DeleteTenantAttachmentPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantAttachment` that was deleted by this mutation. */
	tenantAttachment?: GraphQLTypes["TenantAttachment"],
	deletedTenantAttachmentNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantAttachment`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Attachment` that is related to this `TenantAttachment`. */
	attachmentByObjectName?: GraphQLTypes["Attachment"],
	/** An edge for our `TenantAttachment`. May be used by Relay 1. */
	tenantAttachmentEdge?: GraphQLTypes["TenantAttachmentsEdge"]
};
	/** All input for the `deleteTenantAttachment` mutation. */
["DeleteTenantAttachmentInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	tenantId: GraphQLTypes["BigInt"],
	objectName: string
};
	/** The output of our delete `TenantLocation` mutation. */
["DeleteTenantLocationPayload"]: {
	__typename: "DeleteTenantLocationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantLocation` that was deleted by this mutation. */
	tenantLocation?: GraphQLTypes["TenantLocation"],
	deletedTenantLocationNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantLocation`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Location` that is related to this `TenantLocation`. */
	location?: GraphQLTypes["Location"],
	/** An edge for our `TenantLocation`. May be used by Relay 1. */
	tenantLocationEdge?: GraphQLTypes["TenantLocationsEdge"]
};
	/** All input for the `deleteTenantLocation` mutation. */
["DeleteTenantLocationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	tenantId: GraphQLTypes["BigInt"],
	locationId: GraphQLTypes["BigInt"]
};
	/** The output of our delete `TenantPerson` mutation. */
["DeleteTenantPersonPayload"]: {
	__typename: "DeleteTenantPersonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** The `TenantPerson` that was deleted by this mutation. */
	tenantPerson?: GraphQLTypes["TenantPerson"],
	deletedTenantPersonNodeId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `Tenant` that is related to this `TenantPerson`. */
	tenant?: GraphQLTypes["Tenant"],
	/** Reads a single `Person` that is related to this `TenantPerson`. */
	person?: GraphQLTypes["Person"],
	/** An edge for our `TenantPerson`. May be used by Relay 1. */
	tenantPersonEdge?: GraphQLTypes["TenantPeopleEdge"]
};
	/** All input for the `deleteTenantPerson` mutation. */
["DeleteTenantPersonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	tenantId: GraphQLTypes["BigInt"],
	personId: GraphQLTypes["BigInt"]
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
	/** All input for the `deleteUser` mutation. */
["DeleteUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	uId: GraphQLTypes["BigInt"]
};
	/** The output of our `bookLesson` mutation. */
["BookLessonPayload"]: {
	__typename: "BookLessonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	rozpisItems?: Array<GraphQLTypes["RozpisItem"]>,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `bookLesson` mutation. */
["BookLessonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	lessonId: GraphQLTypes["BigInt"]
};
	/** The output of our `cancelLesson` mutation. */
["CancelLessonPayload"]: {
	__typename: "CancelLessonPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	rozpisItems?: Array<GraphQLTypes["RozpisItem"]>,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `cancelLesson` mutation. */
["CancelLessonInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	lessonId: GraphQLTypes["BigInt"]
};
	/** The output of our `cancelParticipation` mutation. */
["CancelParticipationPayload"]: {
	__typename: "CancelParticipationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `cancelParticipation` mutation. */
["CancelParticipationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	eventId: GraphQLTypes["BigInt"]
};
	/** The output of our `changePassword` mutation. */
["ChangePasswordPayload"]: {
	__typename: "ChangePasswordPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `changePassword` mutation. */
["ChangePasswordInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	oldPass: string,
	newPass: string
};
	/** The output of our `confirmUser` mutation. */
["ConfirmUserPayload"]: {
	__typename: "ConfirmUserPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `confirmUser` mutation. */
["ConfirmUserInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	id: GraphQLTypes["BigInt"],
	grp: GraphQLTypes["BigInt"],
	cohort: GraphQLTypes["BigInt"]
};
	/** The output of our `createCouple` mutation. */
["CreateCouplePayload"]: {
	__typename: "CreateCouplePayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	paries?: Array<GraphQLTypes["Pary"]>,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `createCouple` mutation. */
["CreateCoupleInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	man: GraphQLTypes["BigInt"],
	woman: GraphQLTypes["BigInt"]
};
	/** The output of our `createParticipation` mutation. */
["CreateParticipationPayload"]: {
	__typename: "CreateParticipationPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `createParticipation` mutation. */
["CreateParticipationInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	eventId: GraphQLTypes["BigInt"],
	yearOfBirth: number,
	myNotes: string
};
	/** The output of our `createParticipationExternal` mutation. */
["CreateParticipationExternalPayload"]: {
	__typename: "CreateParticipationExternalPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `createParticipationExternal` mutation. */
["CreateParticipationExternalInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	eventId: GraphQLTypes["BigInt"],
	firstName: string,
	lastName: string,
	guardianName: string,
	email: string,
	phone: string,
	notes: string,
	birthNumber: string
};
	/** The output of our `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesPayload"]: {
	__typename: "FixUnpairedCouplesPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	paries?: Array<GraphQLTypes["Pary"]>,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `fixUnpairedCouples` mutation. */
["FixUnpairedCouplesInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string
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
	/** The output of our `prospectFormDancer` mutation. */
["ProspectFormDancerPayload"]: {
	__typename: "ProspectFormDancerPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `prospectFormDancer` mutation. */
["ProspectFormDancerInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	cohort: GraphQLTypes["CrmCohort"],
	prospectData: GraphQLTypes["ProspectDatumInput"],
	origin: string,
	note: string
};
	["CrmCohort"]: CrmCohort;
	/** An input for mutations affecting `ProspectDatum` */
["ProspectDatumInput"]: {
		name?: string,
	surname?: string,
	email?: string,
	phone?: string,
	yearofbirth?: string
};
	/** The output of our `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsPayload"]: {
	__typename: "ReservationSetDesiredLessonsPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	reservation?: GraphQLTypes["Nabidka"],
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"],
	/** Reads a single `User` that is related to this `Nabidka`. */
	userByNTrener?: GraphQLTypes["User"],
	/** An edge for our `Nabidka`. May be used by Relay 1. */
	nabidkaEdge?: GraphQLTypes["NabidkasEdge"]
};
	/** All input for the `reservationSetDesiredLessons` mutation. */
["ReservationSetDesiredLessonsInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	reservationId: GraphQLTypes["BigInt"],
	lessonCount: number
};
	/** The output of our `resetPassword` mutation. */
["ResetPasswordPayload"]: {
	__typename: "ResetPasswordPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `resetPassword` mutation. */
["ResetPasswordInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	login: string,
	email: string
};
	/** The output of our `submitForm` mutation. */
["SubmitFormPayload"]: {
	__typename: "SubmitFormPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `submitForm` mutation. */
["SubmitFormInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	type: string,
	data: GraphQLTypes["JSON"],
	url: string
};
	/** The output of our `verifyFunction` mutation. */
["VerifyFunctionPayload"]: {
	__typename: "VerifyFunctionPayload",
	/** The exact same `clientMutationId` that was provided in the mutation input,
unchanged and unused. May be used by a client to track mutations. */
	clientMutationId?: string,
	/** Our root query field type. Allows us to run any query from our mutation payload. */
	query?: GraphQLTypes["Query"]
};
	/** All input for the `verifyFunction` mutation. */
["VerifyFunctionInput"]: {
		/** An arbitrary string value with no semantic meaning. Will be included in the
payload verbatim. May be used to track mutations by the client. */
	clientMutationId?: string,
	f?: GraphQLTypes["RegProc"],
	relid?: GraphQLTypes["RegClass"]
};
	/** A builtin object identifier type for a function name */
["RegProc"]:any;
	/** A builtin object identifier type for a relation name */
["RegClass"]:any;
	["UploadFilePayload"]: {
	__typename: "UploadFilePayload",
	uploadUrl: string,
	objectName: string
}
    }
/** Methods to use when ordering `User`. */
export const enum UsersOrderBy {
	NATURAL = "NATURAL",
	U_ID_ASC = "U_ID_ASC",
	U_ID_DESC = "U_ID_DESC",
	U_LOGIN_ASC = "U_LOGIN_ASC",
	U_LOGIN_DESC = "U_LOGIN_DESC",
	U_JMENO_ASC = "U_JMENO_ASC",
	U_JMENO_DESC = "U_JMENO_DESC",
	U_PRIJMENI_ASC = "U_PRIJMENI_ASC",
	U_PRIJMENI_DESC = "U_PRIJMENI_DESC",
	U_NAROZENI_ASC = "U_NAROZENI_ASC",
	U_NAROZENI_DESC = "U_NAROZENI_DESC",
	U_GROUP_ASC = "U_GROUP_ASC",
	U_GROUP_DESC = "U_GROUP_DESC",
	U_SKUPINA_ASC = "U_SKUPINA_ASC",
	U_SKUPINA_DESC = "U_SKUPINA_DESC",
	U_BAN_ASC = "U_BAN_ASC",
	U_BAN_DESC = "U_BAN_DESC",
	U_CONFIRMED_ASC = "U_CONFIRMED_ASC",
	U_CONFIRMED_DESC = "U_CONFIRMED_DESC",
	U_SYSTEM_ASC = "U_SYSTEM_ASC",
	U_SYSTEM_DESC = "U_SYSTEM_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	PERMISSION_BY_U_GROUP__PE_ID_ASC = "PERMISSION_BY_U_GROUP__PE_ID_ASC",
	PERMISSION_BY_U_GROUP__PE_ID_DESC = "PERMISSION_BY_U_GROUP__PE_ID_DESC",
	SKUPINY_BY_U_SKUPINA__S_ID_ASC = "SKUPINY_BY_U_SKUPINA__S_ID_ASC",
	SKUPINY_BY_U_SKUPINA__S_ID_DESC = "SKUPINY_BY_U_SKUPINA__S_ID_DESC",
	SKUPINY_BY_U_SKUPINA__S_VISIBLE_ASC = "SKUPINY_BY_U_SKUPINA__S_VISIBLE_ASC",
	SKUPINY_BY_U_SKUPINA__S_VISIBLE_DESC = "SKUPINY_BY_U_SKUPINA__S_VISIBLE_DESC",
	SKUPINY_BY_U_SKUPINA__ORDERING_ASC = "SKUPINY_BY_U_SKUPINA__ORDERING_ASC",
	SKUPINY_BY_U_SKUPINA__ORDERING_DESC = "SKUPINY_BY_U_SKUPINA__ORDERING_DESC",
	SKUPINY_BY_U_SKUPINA__COHORT_GROUP_ASC = "SKUPINY_BY_U_SKUPINA__COHORT_GROUP_ASC",
	SKUPINY_BY_U_SKUPINA__COHORT_GROUP_DESC = "SKUPINY_BY_U_SKUPINA__COHORT_GROUP_DESC",
	ATTENDEE_USERS_BY_USER_ID__COUNT_ASC = "ATTENDEE_USERS_BY_USER_ID__COUNT_ASC",
	ATTENDEE_USERS_BY_USER_ID__COUNT_DESC = "ATTENDEE_USERS_BY_USER_ID__COUNT_DESC",
	AKTUALITIES_BY_AT_KDO__COUNT_ASC = "AKTUALITIES_BY_AT_KDO__COUNT_ASC",
	AKTUALITIES_BY_AT_KDO__COUNT_DESC = "AKTUALITIES_BY_AT_KDO__COUNT_DESC",
	DOKUMENTIES_BY_D_KDO__COUNT_ASC = "DOKUMENTIES_BY_D_KDO__COUNT_ASC",
	DOKUMENTIES_BY_D_KDO__COUNT_DESC = "DOKUMENTIES_BY_D_KDO__COUNT_DESC",
	GALERIE_FOTOS_BY_GF_KDO__COUNT_ASC = "GALERIE_FOTOS_BY_GF_KDO__COUNT_ASC",
	GALERIE_FOTOS_BY_GF_KDO__COUNT_DESC = "GALERIE_FOTOS_BY_GF_KDO__COUNT_DESC",
	PLATBY_ITEMS_BY_PI_ID_USER__COUNT_ASC = "PLATBY_ITEMS_BY_PI_ID_USER__COUNT_ASC",
	PLATBY_ITEMS_BY_PI_ID_USER__COUNT_DESC = "PLATBY_ITEMS_BY_PI_ID_USER__COUNT_DESC",
	NABIDKAS_BY_N_TRENER__COUNT_ASC = "NABIDKAS_BY_N_TRENER__COUNT_ASC",
	NABIDKAS_BY_N_TRENER__COUNT_DESC = "NABIDKAS_BY_N_TRENER__COUNT_DESC",
	PARIES_BY_P_ID_PARTNER__COUNT_ASC = "PARIES_BY_P_ID_PARTNER__COUNT_ASC",
	PARIES_BY_P_ID_PARTNER__COUNT_DESC = "PARIES_BY_P_ID_PARTNER__COUNT_DESC",
	PARY_NAVRHS_BY_PN_NAVRHL__COUNT_ASC = "PARY_NAVRHS_BY_PN_NAVRHL__COUNT_ASC",
	PARY_NAVRHS_BY_PN_NAVRHL__COUNT_DESC = "PARY_NAVRHS_BY_PN_NAVRHL__COUNT_DESC",
	PARY_NAVRHS_BY_PN_PARTNER__COUNT_ASC = "PARY_NAVRHS_BY_PN_PARTNER__COUNT_ASC",
	PARY_NAVRHS_BY_PN_PARTNER__COUNT_DESC = "PARY_NAVRHS_BY_PN_PARTNER__COUNT_DESC",
	PARY_NAVRHS_BY_PN_PARTNERKA__COUNT_ASC = "PARY_NAVRHS_BY_PN_PARTNERKA__COUNT_ASC",
	PARY_NAVRHS_BY_PN_PARTNERKA__COUNT_DESC = "PARY_NAVRHS_BY_PN_PARTNERKA__COUNT_DESC",
	ROZPIS_BY_R_TRENER__COUNT_ASC = "ROZPIS_BY_R_TRENER__COUNT_ASC",
	ROZPIS_BY_R_TRENER__COUNT_DESC = "ROZPIS_BY_R_TRENER__COUNT_DESC",
	SESSIONS_BY_SS_USER__COUNT_ASC = "SESSIONS_BY_SS_USER__COUNT_ASC",
	SESSIONS_BY_SS_USER__COUNT_DESC = "SESSIONS_BY_SS_USER__COUNT_DESC",
	UPOZORNENIS_BY_UP_KDO__COUNT_ASC = "UPOZORNENIS_BY_UP_KDO__COUNT_ASC",
	UPOZORNENIS_BY_UP_KDO__COUNT_DESC = "UPOZORNENIS_BY_UP_KDO__COUNT_DESC",
	ATTACHMENTS_BY_UPLOADED_BY__COUNT_ASC = "ATTACHMENTS_BY_UPLOADED_BY__COUNT_ASC",
	ATTACHMENTS_BY_UPLOADED_BY__COUNT_DESC = "ATTACHMENTS_BY_UPLOADED_BY__COUNT_DESC",
	ATTENDEE_EXTERNALS_BY_MANAGED_BY__COUNT_ASC = "ATTENDEE_EXTERNALS_BY_MANAGED_BY__COUNT_ASC",
	ATTENDEE_EXTERNALS_BY_MANAGED_BY__COUNT_DESC = "ATTENDEE_EXTERNALS_BY_MANAGED_BY__COUNT_DESC",
	ATTENDEE_EXTERNALS_BY_CONFIRMED_BY__COUNT_ASC = "ATTENDEE_EXTERNALS_BY_CONFIRMED_BY__COUNT_ASC",
	ATTENDEE_EXTERNALS_BY_CONFIRMED_BY__COUNT_DESC = "ATTENDEE_EXTERNALS_BY_CONFIRMED_BY__COUNT_DESC",
	AKCE_ITEMS_BY_AI_USER__COUNT_ASC = "AKCE_ITEMS_BY_AI_USER__COUNT_ASC",
	AKCE_ITEMS_BY_AI_USER__COUNT_DESC = "AKCE_ITEMS_BY_AI_USER__COUNT_DESC",
	PARIES_BY_P_ID_PARTNERKA__COUNT_ASC = "PARIES_BY_P_ID_PARTNERKA__COUNT_ASC",
	PARIES_BY_P_ID_PARTNERKA__COUNT_DESC = "PARIES_BY_P_ID_PARTNERKA__COUNT_DESC"
}
export const enum TenantAttachmentType {
	LOGO = "LOGO",
	PHOTO = "PHOTO",
	MAP = "MAP"
}
/** Methods to use when ordering `TenantAttachment`. */
export const enum TenantAttachmentsOrderBy {
	NATURAL = "NATURAL",
	TENANT_ID_ASC = "TENANT_ID_ASC",
	TENANT_ID_DESC = "TENANT_ID_DESC",
	OBJECT_NAME_ASC = "OBJECT_NAME_ASC",
	OBJECT_NAME_DESC = "OBJECT_NAME_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_BY_TENANT_ID__ID_ASC = "TENANT_BY_TENANT_ID__ID_ASC",
	TENANT_BY_TENANT_ID__ID_DESC = "TENANT_BY_TENANT_ID__ID_DESC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC"
}
/** Methods to use when ordering `RoomAttachment`. */
export const enum RoomAttachmentsOrderBy {
	NATURAL = "NATURAL",
	ROOM_ID_ASC = "ROOM_ID_ASC",
	ROOM_ID_DESC = "ROOM_ID_DESC",
	OBJECT_NAME_ASC = "OBJECT_NAME_ASC",
	OBJECT_NAME_DESC = "OBJECT_NAME_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	ROOM_BY_ROOM_ID__ID_ASC = "ROOM_BY_ROOM_ID__ID_ASC",
	ROOM_BY_ROOM_ID__ID_DESC = "ROOM_BY_ROOM_ID__ID_DESC",
	ROOM_BY_ROOM_ID__LOCATION_ASC = "ROOM_BY_ROOM_ID__LOCATION_ASC",
	ROOM_BY_ROOM_ID__LOCATION_DESC = "ROOM_BY_ROOM_ID__LOCATION_DESC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC"
}
/** Methods to use when ordering `Room`. */
export const enum RoomsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	LOCATION_ASC = "LOCATION_ASC",
	LOCATION_DESC = "LOCATION_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	LOCATION_BY_LOCATION__ID_ASC = "LOCATION_BY_LOCATION__ID_ASC",
	LOCATION_BY_LOCATION__ID_DESC = "LOCATION_BY_LOCATION__ID_DESC",
	ROOM_ATTACHMENTS_BY_ROOM_ID__COUNT_ASC = "ROOM_ATTACHMENTS_BY_ROOM_ID__COUNT_ASC",
	ROOM_ATTACHMENTS_BY_ROOM_ID__COUNT_DESC = "ROOM_ATTACHMENTS_BY_ROOM_ID__COUNT_DESC"
}
/** Methods to use when ordering `LocationAttachment`. */
export const enum LocationAttachmentsOrderBy {
	NATURAL = "NATURAL",
	LOCATION_ID_ASC = "LOCATION_ID_ASC",
	LOCATION_ID_DESC = "LOCATION_ID_DESC",
	OBJECT_NAME_ASC = "OBJECT_NAME_ASC",
	OBJECT_NAME_DESC = "OBJECT_NAME_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	LOCATION_BY_LOCATION_ID__ID_ASC = "LOCATION_BY_LOCATION_ID__ID_ASC",
	LOCATION_BY_LOCATION_ID__ID_DESC = "LOCATION_BY_LOCATION_ID__ID_DESC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_ASC",
	ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC = "ATTACHMENT_BY_OBJECT_NAME__OBJECT_NAME_DESC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_ASC",
	ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC = "ATTACHMENT_BY_OBJECT_NAME__UPLOADED_BY_DESC"
}
/** Methods to use when ordering `TenantLocation`. */
export const enum TenantLocationsOrderBy {
	NATURAL = "NATURAL",
	TENANT_ID_ASC = "TENANT_ID_ASC",
	TENANT_ID_DESC = "TENANT_ID_DESC",
	LOCATION_ID_ASC = "LOCATION_ID_ASC",
	LOCATION_ID_DESC = "LOCATION_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_BY_TENANT_ID__ID_ASC = "TENANT_BY_TENANT_ID__ID_ASC",
	TENANT_BY_TENANT_ID__ID_DESC = "TENANT_BY_TENANT_ID__ID_DESC",
	LOCATION_BY_LOCATION_ID__ID_ASC = "LOCATION_BY_LOCATION_ID__ID_ASC",
	LOCATION_BY_LOCATION_ID__ID_DESC = "LOCATION_BY_LOCATION_ID__ID_DESC"
}
export const enum GenderType {
	MEN = "MEN",
	WOMAN = "WOMAN",
	UNSPECIFIED = "UNSPECIFIED"
}
/** Methods to use when ordering `TenantPerson`. */
export const enum TenantPeopleOrderBy {
	NATURAL = "NATURAL",
	TENANT_ID_ASC = "TENANT_ID_ASC",
	TENANT_ID_DESC = "TENANT_ID_DESC",
	PERSON_ID_ASC = "PERSON_ID_ASC",
	PERSON_ID_DESC = "PERSON_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_BY_TENANT_ID__ID_ASC = "TENANT_BY_TENANT_ID__ID_ASC",
	TENANT_BY_TENANT_ID__ID_DESC = "TENANT_BY_TENANT_ID__ID_DESC",
	PERSON_BY_PERSON_ID__ID_ASC = "PERSON_BY_PERSON_ID__ID_ASC",
	PERSON_BY_PERSON_ID__ID_DESC = "PERSON_BY_PERSON_ID__ID_DESC"
}
/** Methods to use when ordering `CohortGroup`. */
export const enum CohortGroupsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	ORDERING_ASC = "ORDERING_ASC",
	ORDERING_DESC = "ORDERING_DESC",
	IS_PUBLIC_ASC = "IS_PUBLIC_ASC",
	IS_PUBLIC_DESC = "IS_PUBLIC_DESC",
	TENANT_ASC = "TENANT_ASC",
	TENANT_DESC = "TENANT_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_BY_TENANT__ID_ASC = "TENANT_BY_TENANT__ID_ASC",
	TENANT_BY_TENANT__ID_DESC = "TENANT_BY_TENANT__ID_DESC",
	SKUPINIES_BY_COHORT_GROUP__COUNT_ASC = "SKUPINIES_BY_COHORT_GROUP__COUNT_ASC",
	SKUPINIES_BY_COHORT_GROUP__COUNT_DESC = "SKUPINIES_BY_COHORT_GROUP__COUNT_DESC"
}
/** Methods to use when ordering `Skupiny`. */
export const enum SkupiniesOrderBy {
	NATURAL = "NATURAL",
	S_ID_ASC = "S_ID_ASC",
	S_ID_DESC = "S_ID_DESC",
	S_VISIBLE_ASC = "S_VISIBLE_ASC",
	S_VISIBLE_DESC = "S_VISIBLE_DESC",
	ORDERING_ASC = "ORDERING_ASC",
	ORDERING_DESC = "ORDERING_DESC",
	COHORT_GROUP_ASC = "COHORT_GROUP_ASC",
	COHORT_GROUP_DESC = "COHORT_GROUP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	COHORT_GROUP_BY_COHORT_GROUP__ID_ASC = "COHORT_GROUP_BY_COHORT_GROUP__ID_ASC",
	COHORT_GROUP_BY_COHORT_GROUP__ID_DESC = "COHORT_GROUP_BY_COHORT_GROUP__ID_DESC",
	COHORT_GROUP_BY_COHORT_GROUP__ORDERING_ASC = "COHORT_GROUP_BY_COHORT_GROUP__ORDERING_ASC",
	COHORT_GROUP_BY_COHORT_GROUP__ORDERING_DESC = "COHORT_GROUP_BY_COHORT_GROUP__ORDERING_DESC",
	COHORT_GROUP_BY_COHORT_GROUP__IS_PUBLIC_ASC = "COHORT_GROUP_BY_COHORT_GROUP__IS_PUBLIC_ASC",
	COHORT_GROUP_BY_COHORT_GROUP__IS_PUBLIC_DESC = "COHORT_GROUP_BY_COHORT_GROUP__IS_PUBLIC_DESC",
	COHORT_GROUP_BY_COHORT_GROUP__TENANT_ASC = "COHORT_GROUP_BY_COHORT_GROUP__TENANT_ASC",
	COHORT_GROUP_BY_COHORT_GROUP__TENANT_DESC = "COHORT_GROUP_BY_COHORT_GROUP__TENANT_DESC",
	USERS_BY_U_SKUPINA__COUNT_ASC = "USERS_BY_U_SKUPINA__COUNT_ASC",
	USERS_BY_U_SKUPINA__COUNT_DESC = "USERS_BY_U_SKUPINA__COUNT_DESC",
	PLATBY_GROUP_SKUPINAS_BY_PGS_ID_SKUPINA__COUNT_ASC = "PLATBY_GROUP_SKUPINAS_BY_PGS_ID_SKUPINA__COUNT_ASC",
	PLATBY_GROUP_SKUPINAS_BY_PGS_ID_SKUPINA__COUNT_DESC = "PLATBY_GROUP_SKUPINAS_BY_PGS_ID_SKUPINA__COUNT_DESC",
	UPOZORNENI_SKUPINIES_BY_UPS_ID_SKUPINA__COUNT_ASC = "UPOZORNENI_SKUPINIES_BY_UPS_ID_SKUPINA__COUNT_ASC",
	UPOZORNENI_SKUPINIES_BY_UPS_ID_SKUPINA__COUNT_DESC = "UPOZORNENI_SKUPINIES_BY_UPS_ID_SKUPINA__COUNT_DESC"
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
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	PLATBY_GROUP_BY_PCG_ID_GROUP__PG_ID_ASC = "PLATBY_GROUP_BY_PCG_ID_GROUP__PG_ID_ASC",
	PLATBY_GROUP_BY_PCG_ID_GROUP__PG_ID_DESC = "PLATBY_GROUP_BY_PCG_ID_GROUP__PG_ID_DESC",
	PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_ID_ASC = "PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_ID_ASC",
	PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_ID_DESC = "PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_ID_DESC",
	PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_SYMBOL_ASC = "PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_SYMBOL_ASC",
	PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_SYMBOL_DESC = "PLATBY_CATEGORY_BY_PCG_ID_CATEGORY__PC_SYMBOL_DESC"
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
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_PI_ID_USER__U_ID_ASC = "USER_BY_PI_ID_USER__U_ID_ASC",
	USER_BY_PI_ID_USER__U_ID_DESC = "USER_BY_PI_ID_USER__U_ID_DESC",
	USER_BY_PI_ID_USER__U_LOGIN_ASC = "USER_BY_PI_ID_USER__U_LOGIN_ASC",
	USER_BY_PI_ID_USER__U_LOGIN_DESC = "USER_BY_PI_ID_USER__U_LOGIN_DESC",
	USER_BY_PI_ID_USER__U_JMENO_ASC = "USER_BY_PI_ID_USER__U_JMENO_ASC",
	USER_BY_PI_ID_USER__U_JMENO_DESC = "USER_BY_PI_ID_USER__U_JMENO_DESC",
	USER_BY_PI_ID_USER__U_PRIJMENI_ASC = "USER_BY_PI_ID_USER__U_PRIJMENI_ASC",
	USER_BY_PI_ID_USER__U_PRIJMENI_DESC = "USER_BY_PI_ID_USER__U_PRIJMENI_DESC",
	USER_BY_PI_ID_USER__U_NAROZENI_ASC = "USER_BY_PI_ID_USER__U_NAROZENI_ASC",
	USER_BY_PI_ID_USER__U_NAROZENI_DESC = "USER_BY_PI_ID_USER__U_NAROZENI_DESC",
	USER_BY_PI_ID_USER__U_GROUP_ASC = "USER_BY_PI_ID_USER__U_GROUP_ASC",
	USER_BY_PI_ID_USER__U_GROUP_DESC = "USER_BY_PI_ID_USER__U_GROUP_DESC",
	USER_BY_PI_ID_USER__U_SKUPINA_ASC = "USER_BY_PI_ID_USER__U_SKUPINA_ASC",
	USER_BY_PI_ID_USER__U_SKUPINA_DESC = "USER_BY_PI_ID_USER__U_SKUPINA_DESC",
	USER_BY_PI_ID_USER__U_BAN_ASC = "USER_BY_PI_ID_USER__U_BAN_ASC",
	USER_BY_PI_ID_USER__U_BAN_DESC = "USER_BY_PI_ID_USER__U_BAN_DESC",
	USER_BY_PI_ID_USER__U_CONFIRMED_ASC = "USER_BY_PI_ID_USER__U_CONFIRMED_ASC",
	USER_BY_PI_ID_USER__U_CONFIRMED_DESC = "USER_BY_PI_ID_USER__U_CONFIRMED_DESC",
	USER_BY_PI_ID_USER__U_SYSTEM_ASC = "USER_BY_PI_ID_USER__U_SYSTEM_ASC",
	USER_BY_PI_ID_USER__U_SYSTEM_DESC = "USER_BY_PI_ID_USER__U_SYSTEM_DESC",
	PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_ID_ASC = "PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_ID_ASC",
	PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_ID_DESC = "PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_ID_DESC",
	PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_SYMBOL_ASC = "PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_SYMBOL_ASC",
	PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_SYMBOL_DESC = "PLATBY_CATEGORY_BY_PI_ID_CATEGORY__PC_SYMBOL_DESC",
	PLATBY_RAW_BY_PI_ID_RAW__PR_ID_ASC = "PLATBY_RAW_BY_PI_ID_RAW__PR_ID_ASC",
	PLATBY_RAW_BY_PI_ID_RAW__PR_ID_DESC = "PLATBY_RAW_BY_PI_ID_RAW__PR_ID_DESC",
	PLATBY_RAW_BY_PI_ID_RAW__PR_HASH_ASC = "PLATBY_RAW_BY_PI_ID_RAW__PR_HASH_ASC",
	PLATBY_RAW_BY_PI_ID_RAW__PR_HASH_DESC = "PLATBY_RAW_BY_PI_ID_RAW__PR_HASH_DESC"
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
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	SKUPINY_BY_PGS_ID_SKUPINA__S_ID_ASC = "SKUPINY_BY_PGS_ID_SKUPINA__S_ID_ASC",
	SKUPINY_BY_PGS_ID_SKUPINA__S_ID_DESC = "SKUPINY_BY_PGS_ID_SKUPINA__S_ID_DESC",
	SKUPINY_BY_PGS_ID_SKUPINA__S_VISIBLE_ASC = "SKUPINY_BY_PGS_ID_SKUPINA__S_VISIBLE_ASC",
	SKUPINY_BY_PGS_ID_SKUPINA__S_VISIBLE_DESC = "SKUPINY_BY_PGS_ID_SKUPINA__S_VISIBLE_DESC",
	SKUPINY_BY_PGS_ID_SKUPINA__ORDERING_ASC = "SKUPINY_BY_PGS_ID_SKUPINA__ORDERING_ASC",
	SKUPINY_BY_PGS_ID_SKUPINA__ORDERING_DESC = "SKUPINY_BY_PGS_ID_SKUPINA__ORDERING_DESC",
	SKUPINY_BY_PGS_ID_SKUPINA__COHORT_GROUP_ASC = "SKUPINY_BY_PGS_ID_SKUPINA__COHORT_GROUP_ASC",
	SKUPINY_BY_PGS_ID_SKUPINA__COHORT_GROUP_DESC = "SKUPINY_BY_PGS_ID_SKUPINA__COHORT_GROUP_DESC",
	PLATBY_GROUP_BY_PGS_ID_GROUP__PG_ID_ASC = "PLATBY_GROUP_BY_PGS_ID_GROUP__PG_ID_ASC",
	PLATBY_GROUP_BY_PGS_ID_GROUP__PG_ID_DESC = "PLATBY_GROUP_BY_PGS_ID_GROUP__PG_ID_DESC"
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
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_ID_ASC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_ID_ASC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_ID_DESC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_ID_DESC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_KDO_ASC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_KDO_ASC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_KDO_DESC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_KDO_DESC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_TIMESTAMP_ADD_ASC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_TIMESTAMP_ADD_ASC",
	UPOZORNENI_BY_UPS_ID_RODIC__UP_TIMESTAMP_ADD_DESC = "UPOZORNENI_BY_UPS_ID_RODIC__UP_TIMESTAMP_ADD_DESC",
	SKUPINY_BY_UPS_ID_SKUPINA__S_ID_ASC = "SKUPINY_BY_UPS_ID_SKUPINA__S_ID_ASC",
	SKUPINY_BY_UPS_ID_SKUPINA__S_ID_DESC = "SKUPINY_BY_UPS_ID_SKUPINA__S_ID_DESC",
	SKUPINY_BY_UPS_ID_SKUPINA__S_VISIBLE_ASC = "SKUPINY_BY_UPS_ID_SKUPINA__S_VISIBLE_ASC",
	SKUPINY_BY_UPS_ID_SKUPINA__S_VISIBLE_DESC = "SKUPINY_BY_UPS_ID_SKUPINA__S_VISIBLE_DESC",
	SKUPINY_BY_UPS_ID_SKUPINA__ORDERING_ASC = "SKUPINY_BY_UPS_ID_SKUPINA__ORDERING_ASC",
	SKUPINY_BY_UPS_ID_SKUPINA__ORDERING_DESC = "SKUPINY_BY_UPS_ID_SKUPINA__ORDERING_DESC",
	SKUPINY_BY_UPS_ID_SKUPINA__COHORT_GROUP_ASC = "SKUPINY_BY_UPS_ID_SKUPINA__COHORT_GROUP_ASC",
	SKUPINY_BY_UPS_ID_SKUPINA__COHORT_GROUP_DESC = "SKUPINY_BY_UPS_ID_SKUPINA__COHORT_GROUP_DESC"
}
/** Methods to use when ordering `AttendeeUser`. */
export const enum AttendeeUsersOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	EVENT_ID_ASC = "EVENT_ID_ASC",
	EVENT_ID_DESC = "EVENT_ID_DESC",
	USER_ID_ASC = "USER_ID_ASC",
	USER_ID_DESC = "USER_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	EVENT_BY_EVENT_ID__ID_ASC = "EVENT_BY_EVENT_ID__ID_ASC",
	EVENT_BY_EVENT_ID__ID_DESC = "EVENT_BY_EVENT_ID__ID_DESC",
	EVENT_BY_EVENT_ID__SINCE_ASC = "EVENT_BY_EVENT_ID__SINCE_ASC",
	EVENT_BY_EVENT_ID__SINCE_DESC = "EVENT_BY_EVENT_ID__SINCE_DESC",
	EVENT_BY_EVENT_ID__IS_VISIBLE_ASC = "EVENT_BY_EVENT_ID__IS_VISIBLE_ASC",
	EVENT_BY_EVENT_ID__IS_VISIBLE_DESC = "EVENT_BY_EVENT_ID__IS_VISIBLE_DESC",
	USER_BY_USER_ID__U_ID_ASC = "USER_BY_USER_ID__U_ID_ASC",
	USER_BY_USER_ID__U_ID_DESC = "USER_BY_USER_ID__U_ID_DESC",
	USER_BY_USER_ID__U_LOGIN_ASC = "USER_BY_USER_ID__U_LOGIN_ASC",
	USER_BY_USER_ID__U_LOGIN_DESC = "USER_BY_USER_ID__U_LOGIN_DESC",
	USER_BY_USER_ID__U_JMENO_ASC = "USER_BY_USER_ID__U_JMENO_ASC",
	USER_BY_USER_ID__U_JMENO_DESC = "USER_BY_USER_ID__U_JMENO_DESC",
	USER_BY_USER_ID__U_PRIJMENI_ASC = "USER_BY_USER_ID__U_PRIJMENI_ASC",
	USER_BY_USER_ID__U_PRIJMENI_DESC = "USER_BY_USER_ID__U_PRIJMENI_DESC",
	USER_BY_USER_ID__U_NAROZENI_ASC = "USER_BY_USER_ID__U_NAROZENI_ASC",
	USER_BY_USER_ID__U_NAROZENI_DESC = "USER_BY_USER_ID__U_NAROZENI_DESC",
	USER_BY_USER_ID__U_GROUP_ASC = "USER_BY_USER_ID__U_GROUP_ASC",
	USER_BY_USER_ID__U_GROUP_DESC = "USER_BY_USER_ID__U_GROUP_DESC",
	USER_BY_USER_ID__U_SKUPINA_ASC = "USER_BY_USER_ID__U_SKUPINA_ASC",
	USER_BY_USER_ID__U_SKUPINA_DESC = "USER_BY_USER_ID__U_SKUPINA_DESC",
	USER_BY_USER_ID__U_BAN_ASC = "USER_BY_USER_ID__U_BAN_ASC",
	USER_BY_USER_ID__U_BAN_DESC = "USER_BY_USER_ID__U_BAN_DESC",
	USER_BY_USER_ID__U_CONFIRMED_ASC = "USER_BY_USER_ID__U_CONFIRMED_ASC",
	USER_BY_USER_ID__U_CONFIRMED_DESC = "USER_BY_USER_ID__U_CONFIRMED_DESC",
	USER_BY_USER_ID__U_SYSTEM_ASC = "USER_BY_USER_ID__U_SYSTEM_ASC",
	USER_BY_USER_ID__U_SYSTEM_DESC = "USER_BY_USER_ID__U_SYSTEM_DESC"
}
/** Methods to use when ordering `AttendeeExternal`. */
export const enum AttendeeExternalsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	EVENT_ID_ASC = "EVENT_ID_ASC",
	EVENT_ID_DESC = "EVENT_ID_DESC",
	MANAGED_BY_ASC = "MANAGED_BY_ASC",
	MANAGED_BY_DESC = "MANAGED_BY_DESC",
	CONFIRMED_BY_ASC = "CONFIRMED_BY_ASC",
	CONFIRMED_BY_DESC = "CONFIRMED_BY_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	EVENT_BY_EVENT_ID__ID_ASC = "EVENT_BY_EVENT_ID__ID_ASC",
	EVENT_BY_EVENT_ID__ID_DESC = "EVENT_BY_EVENT_ID__ID_DESC",
	EVENT_BY_EVENT_ID__SINCE_ASC = "EVENT_BY_EVENT_ID__SINCE_ASC",
	EVENT_BY_EVENT_ID__SINCE_DESC = "EVENT_BY_EVENT_ID__SINCE_DESC",
	EVENT_BY_EVENT_ID__IS_VISIBLE_ASC = "EVENT_BY_EVENT_ID__IS_VISIBLE_ASC",
	EVENT_BY_EVENT_ID__IS_VISIBLE_DESC = "EVENT_BY_EVENT_ID__IS_VISIBLE_DESC",
	USER_BY_MANAGED_BY__U_ID_ASC = "USER_BY_MANAGED_BY__U_ID_ASC",
	USER_BY_MANAGED_BY__U_ID_DESC = "USER_BY_MANAGED_BY__U_ID_DESC",
	USER_BY_MANAGED_BY__U_LOGIN_ASC = "USER_BY_MANAGED_BY__U_LOGIN_ASC",
	USER_BY_MANAGED_BY__U_LOGIN_DESC = "USER_BY_MANAGED_BY__U_LOGIN_DESC",
	USER_BY_MANAGED_BY__U_JMENO_ASC = "USER_BY_MANAGED_BY__U_JMENO_ASC",
	USER_BY_MANAGED_BY__U_JMENO_DESC = "USER_BY_MANAGED_BY__U_JMENO_DESC",
	USER_BY_MANAGED_BY__U_PRIJMENI_ASC = "USER_BY_MANAGED_BY__U_PRIJMENI_ASC",
	USER_BY_MANAGED_BY__U_PRIJMENI_DESC = "USER_BY_MANAGED_BY__U_PRIJMENI_DESC",
	USER_BY_MANAGED_BY__U_NAROZENI_ASC = "USER_BY_MANAGED_BY__U_NAROZENI_ASC",
	USER_BY_MANAGED_BY__U_NAROZENI_DESC = "USER_BY_MANAGED_BY__U_NAROZENI_DESC",
	USER_BY_MANAGED_BY__U_GROUP_ASC = "USER_BY_MANAGED_BY__U_GROUP_ASC",
	USER_BY_MANAGED_BY__U_GROUP_DESC = "USER_BY_MANAGED_BY__U_GROUP_DESC",
	USER_BY_MANAGED_BY__U_SKUPINA_ASC = "USER_BY_MANAGED_BY__U_SKUPINA_ASC",
	USER_BY_MANAGED_BY__U_SKUPINA_DESC = "USER_BY_MANAGED_BY__U_SKUPINA_DESC",
	USER_BY_MANAGED_BY__U_BAN_ASC = "USER_BY_MANAGED_BY__U_BAN_ASC",
	USER_BY_MANAGED_BY__U_BAN_DESC = "USER_BY_MANAGED_BY__U_BAN_DESC",
	USER_BY_MANAGED_BY__U_CONFIRMED_ASC = "USER_BY_MANAGED_BY__U_CONFIRMED_ASC",
	USER_BY_MANAGED_BY__U_CONFIRMED_DESC = "USER_BY_MANAGED_BY__U_CONFIRMED_DESC",
	USER_BY_MANAGED_BY__U_SYSTEM_ASC = "USER_BY_MANAGED_BY__U_SYSTEM_ASC",
	USER_BY_MANAGED_BY__U_SYSTEM_DESC = "USER_BY_MANAGED_BY__U_SYSTEM_DESC",
	USER_BY_CONFIRMED_BY__U_ID_ASC = "USER_BY_CONFIRMED_BY__U_ID_ASC",
	USER_BY_CONFIRMED_BY__U_ID_DESC = "USER_BY_CONFIRMED_BY__U_ID_DESC",
	USER_BY_CONFIRMED_BY__U_LOGIN_ASC = "USER_BY_CONFIRMED_BY__U_LOGIN_ASC",
	USER_BY_CONFIRMED_BY__U_LOGIN_DESC = "USER_BY_CONFIRMED_BY__U_LOGIN_DESC",
	USER_BY_CONFIRMED_BY__U_JMENO_ASC = "USER_BY_CONFIRMED_BY__U_JMENO_ASC",
	USER_BY_CONFIRMED_BY__U_JMENO_DESC = "USER_BY_CONFIRMED_BY__U_JMENO_DESC",
	USER_BY_CONFIRMED_BY__U_PRIJMENI_ASC = "USER_BY_CONFIRMED_BY__U_PRIJMENI_ASC",
	USER_BY_CONFIRMED_BY__U_PRIJMENI_DESC = "USER_BY_CONFIRMED_BY__U_PRIJMENI_DESC",
	USER_BY_CONFIRMED_BY__U_NAROZENI_ASC = "USER_BY_CONFIRMED_BY__U_NAROZENI_ASC",
	USER_BY_CONFIRMED_BY__U_NAROZENI_DESC = "USER_BY_CONFIRMED_BY__U_NAROZENI_DESC",
	USER_BY_CONFIRMED_BY__U_GROUP_ASC = "USER_BY_CONFIRMED_BY__U_GROUP_ASC",
	USER_BY_CONFIRMED_BY__U_GROUP_DESC = "USER_BY_CONFIRMED_BY__U_GROUP_DESC",
	USER_BY_CONFIRMED_BY__U_SKUPINA_ASC = "USER_BY_CONFIRMED_BY__U_SKUPINA_ASC",
	USER_BY_CONFIRMED_BY__U_SKUPINA_DESC = "USER_BY_CONFIRMED_BY__U_SKUPINA_DESC",
	USER_BY_CONFIRMED_BY__U_BAN_ASC = "USER_BY_CONFIRMED_BY__U_BAN_ASC",
	USER_BY_CONFIRMED_BY__U_BAN_DESC = "USER_BY_CONFIRMED_BY__U_BAN_DESC",
	USER_BY_CONFIRMED_BY__U_CONFIRMED_ASC = "USER_BY_CONFIRMED_BY__U_CONFIRMED_ASC",
	USER_BY_CONFIRMED_BY__U_CONFIRMED_DESC = "USER_BY_CONFIRMED_BY__U_CONFIRMED_DESC",
	USER_BY_CONFIRMED_BY__U_SYSTEM_ASC = "USER_BY_CONFIRMED_BY__U_SYSTEM_ASC",
	USER_BY_CONFIRMED_BY__U_SYSTEM_DESC = "USER_BY_CONFIRMED_BY__U_SYSTEM_DESC"
}
/** Methods to use when ordering `GalerieFoto`. */
export const enum GalerieFotosOrderBy {
	NATURAL = "NATURAL",
	GF_ID_ASC = "GF_ID_ASC",
	GF_ID_DESC = "GF_ID_DESC",
	GF_ID_RODIC_ASC = "GF_ID_RODIC_ASC",
	GF_ID_RODIC_DESC = "GF_ID_RODIC_DESC",
	GF_KDO_ASC = "GF_KDO_ASC",
	GF_KDO_DESC = "GF_KDO_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_ASC = "GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_ASC",
	GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_DESC = "GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_DESC",
	GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_RODIC_ASC = "GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_RODIC_ASC",
	GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_RODIC_DESC = "GALERIE_DIR_BY_GF_ID_RODIC__GD_ID_RODIC_DESC",
	USER_BY_GF_KDO__U_ID_ASC = "USER_BY_GF_KDO__U_ID_ASC",
	USER_BY_GF_KDO__U_ID_DESC = "USER_BY_GF_KDO__U_ID_DESC",
	USER_BY_GF_KDO__U_LOGIN_ASC = "USER_BY_GF_KDO__U_LOGIN_ASC",
	USER_BY_GF_KDO__U_LOGIN_DESC = "USER_BY_GF_KDO__U_LOGIN_DESC",
	USER_BY_GF_KDO__U_JMENO_ASC = "USER_BY_GF_KDO__U_JMENO_ASC",
	USER_BY_GF_KDO__U_JMENO_DESC = "USER_BY_GF_KDO__U_JMENO_DESC",
	USER_BY_GF_KDO__U_PRIJMENI_ASC = "USER_BY_GF_KDO__U_PRIJMENI_ASC",
	USER_BY_GF_KDO__U_PRIJMENI_DESC = "USER_BY_GF_KDO__U_PRIJMENI_DESC",
	USER_BY_GF_KDO__U_NAROZENI_ASC = "USER_BY_GF_KDO__U_NAROZENI_ASC",
	USER_BY_GF_KDO__U_NAROZENI_DESC = "USER_BY_GF_KDO__U_NAROZENI_DESC",
	USER_BY_GF_KDO__U_GROUP_ASC = "USER_BY_GF_KDO__U_GROUP_ASC",
	USER_BY_GF_KDO__U_GROUP_DESC = "USER_BY_GF_KDO__U_GROUP_DESC",
	USER_BY_GF_KDO__U_SKUPINA_ASC = "USER_BY_GF_KDO__U_SKUPINA_ASC",
	USER_BY_GF_KDO__U_SKUPINA_DESC = "USER_BY_GF_KDO__U_SKUPINA_DESC",
	USER_BY_GF_KDO__U_BAN_ASC = "USER_BY_GF_KDO__U_BAN_ASC",
	USER_BY_GF_KDO__U_BAN_DESC = "USER_BY_GF_KDO__U_BAN_DESC",
	USER_BY_GF_KDO__U_CONFIRMED_ASC = "USER_BY_GF_KDO__U_CONFIRMED_ASC",
	USER_BY_GF_KDO__U_CONFIRMED_DESC = "USER_BY_GF_KDO__U_CONFIRMED_DESC",
	USER_BY_GF_KDO__U_SYSTEM_ASC = "USER_BY_GF_KDO__U_SYSTEM_ASC",
	USER_BY_GF_KDO__U_SYSTEM_DESC = "USER_BY_GF_KDO__U_SYSTEM_DESC",
	AKTUALITIES_BY_AT_FOTO_MAIN__COUNT_ASC = "AKTUALITIES_BY_AT_FOTO_MAIN__COUNT_ASC",
	AKTUALITIES_BY_AT_FOTO_MAIN__COUNT_DESC = "AKTUALITIES_BY_AT_FOTO_MAIN__COUNT_DESC"
}
/** Methods to use when ordering `Aktuality`. */
export const enum AktualitiesOrderBy {
	NATURAL = "NATURAL",
	AT_ID_ASC = "AT_ID_ASC",
	AT_ID_DESC = "AT_ID_DESC",
	AT_KDO_ASC = "AT_KDO_ASC",
	AT_KDO_DESC = "AT_KDO_DESC",
	AT_FOTO_MAIN_ASC = "AT_FOTO_MAIN_ASC",
	AT_FOTO_MAIN_DESC = "AT_FOTO_MAIN_DESC",
	AT_TIMESTAMP_ADD_ASC = "AT_TIMESTAMP_ADD_ASC",
	AT_TIMESTAMP_ADD_DESC = "AT_TIMESTAMP_ADD_DESC",
	TENANT_ID_ASC = "TENANT_ID_ASC",
	TENANT_ID_DESC = "TENANT_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_AT_KDO__U_ID_ASC = "USER_BY_AT_KDO__U_ID_ASC",
	USER_BY_AT_KDO__U_ID_DESC = "USER_BY_AT_KDO__U_ID_DESC",
	USER_BY_AT_KDO__U_LOGIN_ASC = "USER_BY_AT_KDO__U_LOGIN_ASC",
	USER_BY_AT_KDO__U_LOGIN_DESC = "USER_BY_AT_KDO__U_LOGIN_DESC",
	USER_BY_AT_KDO__U_JMENO_ASC = "USER_BY_AT_KDO__U_JMENO_ASC",
	USER_BY_AT_KDO__U_JMENO_DESC = "USER_BY_AT_KDO__U_JMENO_DESC",
	USER_BY_AT_KDO__U_PRIJMENI_ASC = "USER_BY_AT_KDO__U_PRIJMENI_ASC",
	USER_BY_AT_KDO__U_PRIJMENI_DESC = "USER_BY_AT_KDO__U_PRIJMENI_DESC",
	USER_BY_AT_KDO__U_NAROZENI_ASC = "USER_BY_AT_KDO__U_NAROZENI_ASC",
	USER_BY_AT_KDO__U_NAROZENI_DESC = "USER_BY_AT_KDO__U_NAROZENI_DESC",
	USER_BY_AT_KDO__U_GROUP_ASC = "USER_BY_AT_KDO__U_GROUP_ASC",
	USER_BY_AT_KDO__U_GROUP_DESC = "USER_BY_AT_KDO__U_GROUP_DESC",
	USER_BY_AT_KDO__U_SKUPINA_ASC = "USER_BY_AT_KDO__U_SKUPINA_ASC",
	USER_BY_AT_KDO__U_SKUPINA_DESC = "USER_BY_AT_KDO__U_SKUPINA_DESC",
	USER_BY_AT_KDO__U_BAN_ASC = "USER_BY_AT_KDO__U_BAN_ASC",
	USER_BY_AT_KDO__U_BAN_DESC = "USER_BY_AT_KDO__U_BAN_DESC",
	USER_BY_AT_KDO__U_CONFIRMED_ASC = "USER_BY_AT_KDO__U_CONFIRMED_ASC",
	USER_BY_AT_KDO__U_CONFIRMED_DESC = "USER_BY_AT_KDO__U_CONFIRMED_DESC",
	USER_BY_AT_KDO__U_SYSTEM_ASC = "USER_BY_AT_KDO__U_SYSTEM_ASC",
	USER_BY_AT_KDO__U_SYSTEM_DESC = "USER_BY_AT_KDO__U_SYSTEM_DESC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_ASC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_ASC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_DESC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_DESC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_RODIC_ASC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_RODIC_ASC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_RODIC_DESC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_ID_RODIC_DESC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_KDO_ASC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_KDO_ASC",
	GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_KDO_DESC = "GALERIE_FOTO_BY_AT_FOTO_MAIN__GF_KDO_DESC"
}
/** Methods to use when ordering `Dokumenty`. */
export const enum DokumentiesOrderBy {
	NATURAL = "NATURAL",
	D_ID_ASC = "D_ID_ASC",
	D_ID_DESC = "D_ID_DESC",
	D_PATH_ASC = "D_PATH_ASC",
	D_PATH_DESC = "D_PATH_DESC",
	D_KATEGORIE_ASC = "D_KATEGORIE_ASC",
	D_KATEGORIE_DESC = "D_KATEGORIE_DESC",
	D_KDO_ASC = "D_KDO_ASC",
	D_KDO_DESC = "D_KDO_DESC",
	D_TIMESTAMP_ASC = "D_TIMESTAMP_ASC",
	D_TIMESTAMP_DESC = "D_TIMESTAMP_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_D_KDO__U_ID_ASC = "USER_BY_D_KDO__U_ID_ASC",
	USER_BY_D_KDO__U_ID_DESC = "USER_BY_D_KDO__U_ID_DESC",
	USER_BY_D_KDO__U_LOGIN_ASC = "USER_BY_D_KDO__U_LOGIN_ASC",
	USER_BY_D_KDO__U_LOGIN_DESC = "USER_BY_D_KDO__U_LOGIN_DESC",
	USER_BY_D_KDO__U_JMENO_ASC = "USER_BY_D_KDO__U_JMENO_ASC",
	USER_BY_D_KDO__U_JMENO_DESC = "USER_BY_D_KDO__U_JMENO_DESC",
	USER_BY_D_KDO__U_PRIJMENI_ASC = "USER_BY_D_KDO__U_PRIJMENI_ASC",
	USER_BY_D_KDO__U_PRIJMENI_DESC = "USER_BY_D_KDO__U_PRIJMENI_DESC",
	USER_BY_D_KDO__U_NAROZENI_ASC = "USER_BY_D_KDO__U_NAROZENI_ASC",
	USER_BY_D_KDO__U_NAROZENI_DESC = "USER_BY_D_KDO__U_NAROZENI_DESC",
	USER_BY_D_KDO__U_GROUP_ASC = "USER_BY_D_KDO__U_GROUP_ASC",
	USER_BY_D_KDO__U_GROUP_DESC = "USER_BY_D_KDO__U_GROUP_DESC",
	USER_BY_D_KDO__U_SKUPINA_ASC = "USER_BY_D_KDO__U_SKUPINA_ASC",
	USER_BY_D_KDO__U_SKUPINA_DESC = "USER_BY_D_KDO__U_SKUPINA_DESC",
	USER_BY_D_KDO__U_BAN_ASC = "USER_BY_D_KDO__U_BAN_ASC",
	USER_BY_D_KDO__U_BAN_DESC = "USER_BY_D_KDO__U_BAN_DESC",
	USER_BY_D_KDO__U_CONFIRMED_ASC = "USER_BY_D_KDO__U_CONFIRMED_ASC",
	USER_BY_D_KDO__U_CONFIRMED_DESC = "USER_BY_D_KDO__U_CONFIRMED_DESC",
	USER_BY_D_KDO__U_SYSTEM_ASC = "USER_BY_D_KDO__U_SYSTEM_ASC",
	USER_BY_D_KDO__U_SYSTEM_DESC = "USER_BY_D_KDO__U_SYSTEM_DESC"
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
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	NABIDKA_BY_NI_ID_RODIC__N_ID_ASC = "NABIDKA_BY_NI_ID_RODIC__N_ID_ASC",
	NABIDKA_BY_NI_ID_RODIC__N_ID_DESC = "NABIDKA_BY_NI_ID_RODIC__N_ID_DESC",
	NABIDKA_BY_NI_ID_RODIC__N_TRENER_ASC = "NABIDKA_BY_NI_ID_RODIC__N_TRENER_ASC",
	NABIDKA_BY_NI_ID_RODIC__N_TRENER_DESC = "NABIDKA_BY_NI_ID_RODIC__N_TRENER_DESC",
	NABIDKA_BY_NI_ID_RODIC__N_OD_ASC = "NABIDKA_BY_NI_ID_RODIC__N_OD_ASC",
	NABIDKA_BY_NI_ID_RODIC__N_OD_DESC = "NABIDKA_BY_NI_ID_RODIC__N_OD_DESC",
	PARY_BY_NI_PARTNER__P_ID_ASC = "PARY_BY_NI_PARTNER__P_ID_ASC",
	PARY_BY_NI_PARTNER__P_ID_DESC = "PARY_BY_NI_PARTNER__P_ID_DESC",
	PARY_BY_NI_PARTNER__P_ID_PARTNER_ASC = "PARY_BY_NI_PARTNER__P_ID_PARTNER_ASC",
	PARY_BY_NI_PARTNER__P_ID_PARTNER_DESC = "PARY_BY_NI_PARTNER__P_ID_PARTNER_DESC",
	PARY_BY_NI_PARTNER__P_ID_PARTNERKA_ASC = "PARY_BY_NI_PARTNER__P_ID_PARTNERKA_ASC",
	PARY_BY_NI_PARTNER__P_ID_PARTNERKA_DESC = "PARY_BY_NI_PARTNER__P_ID_PARTNERKA_DESC",
	PARY_BY_NI_PARTNER__P_HODNOCENI_ASC = "PARY_BY_NI_PARTNER__P_HODNOCENI_ASC",
	PARY_BY_NI_PARTNER__P_HODNOCENI_DESC = "PARY_BY_NI_PARTNER__P_HODNOCENI_DESC"
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
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	ROZPI_BY_RI_ID_RODIC__R_ID_ASC = "ROZPI_BY_RI_ID_RODIC__R_ID_ASC",
	ROZPI_BY_RI_ID_RODIC__R_ID_DESC = "ROZPI_BY_RI_ID_RODIC__R_ID_DESC",
	ROZPI_BY_RI_ID_RODIC__R_TRENER_ASC = "ROZPI_BY_RI_ID_RODIC__R_TRENER_ASC",
	ROZPI_BY_RI_ID_RODIC__R_TRENER_DESC = "ROZPI_BY_RI_ID_RODIC__R_TRENER_DESC",
	ROZPI_BY_RI_ID_RODIC__R_DATUM_ASC = "ROZPI_BY_RI_ID_RODIC__R_DATUM_ASC",
	ROZPI_BY_RI_ID_RODIC__R_DATUM_DESC = "ROZPI_BY_RI_ID_RODIC__R_DATUM_DESC",
	PARY_BY_RI_PARTNER__P_ID_ASC = "PARY_BY_RI_PARTNER__P_ID_ASC",
	PARY_BY_RI_PARTNER__P_ID_DESC = "PARY_BY_RI_PARTNER__P_ID_DESC",
	PARY_BY_RI_PARTNER__P_ID_PARTNER_ASC = "PARY_BY_RI_PARTNER__P_ID_PARTNER_ASC",
	PARY_BY_RI_PARTNER__P_ID_PARTNER_DESC = "PARY_BY_RI_PARTNER__P_ID_PARTNER_DESC",
	PARY_BY_RI_PARTNER__P_ID_PARTNERKA_ASC = "PARY_BY_RI_PARTNER__P_ID_PARTNERKA_ASC",
	PARY_BY_RI_PARTNER__P_ID_PARTNERKA_DESC = "PARY_BY_RI_PARTNER__P_ID_PARTNERKA_DESC",
	PARY_BY_RI_PARTNER__P_HODNOCENI_ASC = "PARY_BY_RI_PARTNER__P_HODNOCENI_ASC",
	PARY_BY_RI_PARTNER__P_HODNOCENI_DESC = "PARY_BY_RI_PARTNER__P_HODNOCENI_DESC"
}
/** Methods to use when ordering `Nabidka`. */
export const enum NabidkasOrderBy {
	NATURAL = "NATURAL",
	N_ID_ASC = "N_ID_ASC",
	N_ID_DESC = "N_ID_DESC",
	N_TRENER_ASC = "N_TRENER_ASC",
	N_TRENER_DESC = "N_TRENER_DESC",
	N_OD_ASC = "N_OD_ASC",
	N_OD_DESC = "N_OD_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_N_TRENER__U_ID_ASC = "USER_BY_N_TRENER__U_ID_ASC",
	USER_BY_N_TRENER__U_ID_DESC = "USER_BY_N_TRENER__U_ID_DESC",
	USER_BY_N_TRENER__U_LOGIN_ASC = "USER_BY_N_TRENER__U_LOGIN_ASC",
	USER_BY_N_TRENER__U_LOGIN_DESC = "USER_BY_N_TRENER__U_LOGIN_DESC",
	USER_BY_N_TRENER__U_JMENO_ASC = "USER_BY_N_TRENER__U_JMENO_ASC",
	USER_BY_N_TRENER__U_JMENO_DESC = "USER_BY_N_TRENER__U_JMENO_DESC",
	USER_BY_N_TRENER__U_PRIJMENI_ASC = "USER_BY_N_TRENER__U_PRIJMENI_ASC",
	USER_BY_N_TRENER__U_PRIJMENI_DESC = "USER_BY_N_TRENER__U_PRIJMENI_DESC",
	USER_BY_N_TRENER__U_NAROZENI_ASC = "USER_BY_N_TRENER__U_NAROZENI_ASC",
	USER_BY_N_TRENER__U_NAROZENI_DESC = "USER_BY_N_TRENER__U_NAROZENI_DESC",
	USER_BY_N_TRENER__U_GROUP_ASC = "USER_BY_N_TRENER__U_GROUP_ASC",
	USER_BY_N_TRENER__U_GROUP_DESC = "USER_BY_N_TRENER__U_GROUP_DESC",
	USER_BY_N_TRENER__U_SKUPINA_ASC = "USER_BY_N_TRENER__U_SKUPINA_ASC",
	USER_BY_N_TRENER__U_SKUPINA_DESC = "USER_BY_N_TRENER__U_SKUPINA_DESC",
	USER_BY_N_TRENER__U_BAN_ASC = "USER_BY_N_TRENER__U_BAN_ASC",
	USER_BY_N_TRENER__U_BAN_DESC = "USER_BY_N_TRENER__U_BAN_DESC",
	USER_BY_N_TRENER__U_CONFIRMED_ASC = "USER_BY_N_TRENER__U_CONFIRMED_ASC",
	USER_BY_N_TRENER__U_CONFIRMED_DESC = "USER_BY_N_TRENER__U_CONFIRMED_DESC",
	USER_BY_N_TRENER__U_SYSTEM_ASC = "USER_BY_N_TRENER__U_SYSTEM_ASC",
	USER_BY_N_TRENER__U_SYSTEM_DESC = "USER_BY_N_TRENER__U_SYSTEM_DESC",
	NABIDKA_ITEMS_BY_NI_ID_RODIC__COUNT_ASC = "NABIDKA_ITEMS_BY_NI_ID_RODIC__COUNT_ASC",
	NABIDKA_ITEMS_BY_NI_ID_RODIC__COUNT_DESC = "NABIDKA_ITEMS_BY_NI_ID_RODIC__COUNT_DESC"
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
	P_HODNOCENI_ASC = "P_HODNOCENI_ASC",
	P_HODNOCENI_DESC = "P_HODNOCENI_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_P_ID_PARTNER__U_ID_ASC = "USER_BY_P_ID_PARTNER__U_ID_ASC",
	USER_BY_P_ID_PARTNER__U_ID_DESC = "USER_BY_P_ID_PARTNER__U_ID_DESC",
	USER_BY_P_ID_PARTNER__U_LOGIN_ASC = "USER_BY_P_ID_PARTNER__U_LOGIN_ASC",
	USER_BY_P_ID_PARTNER__U_LOGIN_DESC = "USER_BY_P_ID_PARTNER__U_LOGIN_DESC",
	USER_BY_P_ID_PARTNER__U_JMENO_ASC = "USER_BY_P_ID_PARTNER__U_JMENO_ASC",
	USER_BY_P_ID_PARTNER__U_JMENO_DESC = "USER_BY_P_ID_PARTNER__U_JMENO_DESC",
	USER_BY_P_ID_PARTNER__U_PRIJMENI_ASC = "USER_BY_P_ID_PARTNER__U_PRIJMENI_ASC",
	USER_BY_P_ID_PARTNER__U_PRIJMENI_DESC = "USER_BY_P_ID_PARTNER__U_PRIJMENI_DESC",
	USER_BY_P_ID_PARTNER__U_NAROZENI_ASC = "USER_BY_P_ID_PARTNER__U_NAROZENI_ASC",
	USER_BY_P_ID_PARTNER__U_NAROZENI_DESC = "USER_BY_P_ID_PARTNER__U_NAROZENI_DESC",
	USER_BY_P_ID_PARTNER__U_GROUP_ASC = "USER_BY_P_ID_PARTNER__U_GROUP_ASC",
	USER_BY_P_ID_PARTNER__U_GROUP_DESC = "USER_BY_P_ID_PARTNER__U_GROUP_DESC",
	USER_BY_P_ID_PARTNER__U_SKUPINA_ASC = "USER_BY_P_ID_PARTNER__U_SKUPINA_ASC",
	USER_BY_P_ID_PARTNER__U_SKUPINA_DESC = "USER_BY_P_ID_PARTNER__U_SKUPINA_DESC",
	USER_BY_P_ID_PARTNER__U_BAN_ASC = "USER_BY_P_ID_PARTNER__U_BAN_ASC",
	USER_BY_P_ID_PARTNER__U_BAN_DESC = "USER_BY_P_ID_PARTNER__U_BAN_DESC",
	USER_BY_P_ID_PARTNER__U_CONFIRMED_ASC = "USER_BY_P_ID_PARTNER__U_CONFIRMED_ASC",
	USER_BY_P_ID_PARTNER__U_CONFIRMED_DESC = "USER_BY_P_ID_PARTNER__U_CONFIRMED_DESC",
	USER_BY_P_ID_PARTNER__U_SYSTEM_ASC = "USER_BY_P_ID_PARTNER__U_SYSTEM_ASC",
	USER_BY_P_ID_PARTNER__U_SYSTEM_DESC = "USER_BY_P_ID_PARTNER__U_SYSTEM_DESC",
	USER_BY_P_ID_PARTNERKA__U_ID_ASC = "USER_BY_P_ID_PARTNERKA__U_ID_ASC",
	USER_BY_P_ID_PARTNERKA__U_ID_DESC = "USER_BY_P_ID_PARTNERKA__U_ID_DESC",
	USER_BY_P_ID_PARTNERKA__U_LOGIN_ASC = "USER_BY_P_ID_PARTNERKA__U_LOGIN_ASC",
	USER_BY_P_ID_PARTNERKA__U_LOGIN_DESC = "USER_BY_P_ID_PARTNERKA__U_LOGIN_DESC",
	USER_BY_P_ID_PARTNERKA__U_JMENO_ASC = "USER_BY_P_ID_PARTNERKA__U_JMENO_ASC",
	USER_BY_P_ID_PARTNERKA__U_JMENO_DESC = "USER_BY_P_ID_PARTNERKA__U_JMENO_DESC",
	USER_BY_P_ID_PARTNERKA__U_PRIJMENI_ASC = "USER_BY_P_ID_PARTNERKA__U_PRIJMENI_ASC",
	USER_BY_P_ID_PARTNERKA__U_PRIJMENI_DESC = "USER_BY_P_ID_PARTNERKA__U_PRIJMENI_DESC",
	USER_BY_P_ID_PARTNERKA__U_NAROZENI_ASC = "USER_BY_P_ID_PARTNERKA__U_NAROZENI_ASC",
	USER_BY_P_ID_PARTNERKA__U_NAROZENI_DESC = "USER_BY_P_ID_PARTNERKA__U_NAROZENI_DESC",
	USER_BY_P_ID_PARTNERKA__U_GROUP_ASC = "USER_BY_P_ID_PARTNERKA__U_GROUP_ASC",
	USER_BY_P_ID_PARTNERKA__U_GROUP_DESC = "USER_BY_P_ID_PARTNERKA__U_GROUP_DESC",
	USER_BY_P_ID_PARTNERKA__U_SKUPINA_ASC = "USER_BY_P_ID_PARTNERKA__U_SKUPINA_ASC",
	USER_BY_P_ID_PARTNERKA__U_SKUPINA_DESC = "USER_BY_P_ID_PARTNERKA__U_SKUPINA_DESC",
	USER_BY_P_ID_PARTNERKA__U_BAN_ASC = "USER_BY_P_ID_PARTNERKA__U_BAN_ASC",
	USER_BY_P_ID_PARTNERKA__U_BAN_DESC = "USER_BY_P_ID_PARTNERKA__U_BAN_DESC",
	USER_BY_P_ID_PARTNERKA__U_CONFIRMED_ASC = "USER_BY_P_ID_PARTNERKA__U_CONFIRMED_ASC",
	USER_BY_P_ID_PARTNERKA__U_CONFIRMED_DESC = "USER_BY_P_ID_PARTNERKA__U_CONFIRMED_DESC",
	USER_BY_P_ID_PARTNERKA__U_SYSTEM_ASC = "USER_BY_P_ID_PARTNERKA__U_SYSTEM_ASC",
	USER_BY_P_ID_PARTNERKA__U_SYSTEM_DESC = "USER_BY_P_ID_PARTNERKA__U_SYSTEM_DESC",
	NABIDKA_ITEMS_BY_NI_PARTNER__COUNT_ASC = "NABIDKA_ITEMS_BY_NI_PARTNER__COUNT_ASC",
	NABIDKA_ITEMS_BY_NI_PARTNER__COUNT_DESC = "NABIDKA_ITEMS_BY_NI_PARTNER__COUNT_DESC",
	ROZPIS_ITEMS_BY_RI_PARTNER__COUNT_ASC = "ROZPIS_ITEMS_BY_RI_PARTNER__COUNT_ASC",
	ROZPIS_ITEMS_BY_RI_PARTNER__COUNT_DESC = "ROZPIS_ITEMS_BY_RI_PARTNER__COUNT_DESC"
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
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_PN_NAVRHL__U_ID_ASC = "USER_BY_PN_NAVRHL__U_ID_ASC",
	USER_BY_PN_NAVRHL__U_ID_DESC = "USER_BY_PN_NAVRHL__U_ID_DESC",
	USER_BY_PN_NAVRHL__U_LOGIN_ASC = "USER_BY_PN_NAVRHL__U_LOGIN_ASC",
	USER_BY_PN_NAVRHL__U_LOGIN_DESC = "USER_BY_PN_NAVRHL__U_LOGIN_DESC",
	USER_BY_PN_NAVRHL__U_JMENO_ASC = "USER_BY_PN_NAVRHL__U_JMENO_ASC",
	USER_BY_PN_NAVRHL__U_JMENO_DESC = "USER_BY_PN_NAVRHL__U_JMENO_DESC",
	USER_BY_PN_NAVRHL__U_PRIJMENI_ASC = "USER_BY_PN_NAVRHL__U_PRIJMENI_ASC",
	USER_BY_PN_NAVRHL__U_PRIJMENI_DESC = "USER_BY_PN_NAVRHL__U_PRIJMENI_DESC",
	USER_BY_PN_NAVRHL__U_NAROZENI_ASC = "USER_BY_PN_NAVRHL__U_NAROZENI_ASC",
	USER_BY_PN_NAVRHL__U_NAROZENI_DESC = "USER_BY_PN_NAVRHL__U_NAROZENI_DESC",
	USER_BY_PN_NAVRHL__U_GROUP_ASC = "USER_BY_PN_NAVRHL__U_GROUP_ASC",
	USER_BY_PN_NAVRHL__U_GROUP_DESC = "USER_BY_PN_NAVRHL__U_GROUP_DESC",
	USER_BY_PN_NAVRHL__U_SKUPINA_ASC = "USER_BY_PN_NAVRHL__U_SKUPINA_ASC",
	USER_BY_PN_NAVRHL__U_SKUPINA_DESC = "USER_BY_PN_NAVRHL__U_SKUPINA_DESC",
	USER_BY_PN_NAVRHL__U_BAN_ASC = "USER_BY_PN_NAVRHL__U_BAN_ASC",
	USER_BY_PN_NAVRHL__U_BAN_DESC = "USER_BY_PN_NAVRHL__U_BAN_DESC",
	USER_BY_PN_NAVRHL__U_CONFIRMED_ASC = "USER_BY_PN_NAVRHL__U_CONFIRMED_ASC",
	USER_BY_PN_NAVRHL__U_CONFIRMED_DESC = "USER_BY_PN_NAVRHL__U_CONFIRMED_DESC",
	USER_BY_PN_NAVRHL__U_SYSTEM_ASC = "USER_BY_PN_NAVRHL__U_SYSTEM_ASC",
	USER_BY_PN_NAVRHL__U_SYSTEM_DESC = "USER_BY_PN_NAVRHL__U_SYSTEM_DESC",
	USER_BY_PN_PARTNER__U_ID_ASC = "USER_BY_PN_PARTNER__U_ID_ASC",
	USER_BY_PN_PARTNER__U_ID_DESC = "USER_BY_PN_PARTNER__U_ID_DESC",
	USER_BY_PN_PARTNER__U_LOGIN_ASC = "USER_BY_PN_PARTNER__U_LOGIN_ASC",
	USER_BY_PN_PARTNER__U_LOGIN_DESC = "USER_BY_PN_PARTNER__U_LOGIN_DESC",
	USER_BY_PN_PARTNER__U_JMENO_ASC = "USER_BY_PN_PARTNER__U_JMENO_ASC",
	USER_BY_PN_PARTNER__U_JMENO_DESC = "USER_BY_PN_PARTNER__U_JMENO_DESC",
	USER_BY_PN_PARTNER__U_PRIJMENI_ASC = "USER_BY_PN_PARTNER__U_PRIJMENI_ASC",
	USER_BY_PN_PARTNER__U_PRIJMENI_DESC = "USER_BY_PN_PARTNER__U_PRIJMENI_DESC",
	USER_BY_PN_PARTNER__U_NAROZENI_ASC = "USER_BY_PN_PARTNER__U_NAROZENI_ASC",
	USER_BY_PN_PARTNER__U_NAROZENI_DESC = "USER_BY_PN_PARTNER__U_NAROZENI_DESC",
	USER_BY_PN_PARTNER__U_GROUP_ASC = "USER_BY_PN_PARTNER__U_GROUP_ASC",
	USER_BY_PN_PARTNER__U_GROUP_DESC = "USER_BY_PN_PARTNER__U_GROUP_DESC",
	USER_BY_PN_PARTNER__U_SKUPINA_ASC = "USER_BY_PN_PARTNER__U_SKUPINA_ASC",
	USER_BY_PN_PARTNER__U_SKUPINA_DESC = "USER_BY_PN_PARTNER__U_SKUPINA_DESC",
	USER_BY_PN_PARTNER__U_BAN_ASC = "USER_BY_PN_PARTNER__U_BAN_ASC",
	USER_BY_PN_PARTNER__U_BAN_DESC = "USER_BY_PN_PARTNER__U_BAN_DESC",
	USER_BY_PN_PARTNER__U_CONFIRMED_ASC = "USER_BY_PN_PARTNER__U_CONFIRMED_ASC",
	USER_BY_PN_PARTNER__U_CONFIRMED_DESC = "USER_BY_PN_PARTNER__U_CONFIRMED_DESC",
	USER_BY_PN_PARTNER__U_SYSTEM_ASC = "USER_BY_PN_PARTNER__U_SYSTEM_ASC",
	USER_BY_PN_PARTNER__U_SYSTEM_DESC = "USER_BY_PN_PARTNER__U_SYSTEM_DESC",
	USER_BY_PN_PARTNERKA__U_ID_ASC = "USER_BY_PN_PARTNERKA__U_ID_ASC",
	USER_BY_PN_PARTNERKA__U_ID_DESC = "USER_BY_PN_PARTNERKA__U_ID_DESC",
	USER_BY_PN_PARTNERKA__U_LOGIN_ASC = "USER_BY_PN_PARTNERKA__U_LOGIN_ASC",
	USER_BY_PN_PARTNERKA__U_LOGIN_DESC = "USER_BY_PN_PARTNERKA__U_LOGIN_DESC",
	USER_BY_PN_PARTNERKA__U_JMENO_ASC = "USER_BY_PN_PARTNERKA__U_JMENO_ASC",
	USER_BY_PN_PARTNERKA__U_JMENO_DESC = "USER_BY_PN_PARTNERKA__U_JMENO_DESC",
	USER_BY_PN_PARTNERKA__U_PRIJMENI_ASC = "USER_BY_PN_PARTNERKA__U_PRIJMENI_ASC",
	USER_BY_PN_PARTNERKA__U_PRIJMENI_DESC = "USER_BY_PN_PARTNERKA__U_PRIJMENI_DESC",
	USER_BY_PN_PARTNERKA__U_NAROZENI_ASC = "USER_BY_PN_PARTNERKA__U_NAROZENI_ASC",
	USER_BY_PN_PARTNERKA__U_NAROZENI_DESC = "USER_BY_PN_PARTNERKA__U_NAROZENI_DESC",
	USER_BY_PN_PARTNERKA__U_GROUP_ASC = "USER_BY_PN_PARTNERKA__U_GROUP_ASC",
	USER_BY_PN_PARTNERKA__U_GROUP_DESC = "USER_BY_PN_PARTNERKA__U_GROUP_DESC",
	USER_BY_PN_PARTNERKA__U_SKUPINA_ASC = "USER_BY_PN_PARTNERKA__U_SKUPINA_ASC",
	USER_BY_PN_PARTNERKA__U_SKUPINA_DESC = "USER_BY_PN_PARTNERKA__U_SKUPINA_DESC",
	USER_BY_PN_PARTNERKA__U_BAN_ASC = "USER_BY_PN_PARTNERKA__U_BAN_ASC",
	USER_BY_PN_PARTNERKA__U_BAN_DESC = "USER_BY_PN_PARTNERKA__U_BAN_DESC",
	USER_BY_PN_PARTNERKA__U_CONFIRMED_ASC = "USER_BY_PN_PARTNERKA__U_CONFIRMED_ASC",
	USER_BY_PN_PARTNERKA__U_CONFIRMED_DESC = "USER_BY_PN_PARTNERKA__U_CONFIRMED_DESC",
	USER_BY_PN_PARTNERKA__U_SYSTEM_ASC = "USER_BY_PN_PARTNERKA__U_SYSTEM_ASC",
	USER_BY_PN_PARTNERKA__U_SYSTEM_DESC = "USER_BY_PN_PARTNERKA__U_SYSTEM_DESC"
}
/** Methods to use when ordering `Rozpi`. */
export const enum RozpisOrderBy {
	NATURAL = "NATURAL",
	R_ID_ASC = "R_ID_ASC",
	R_ID_DESC = "R_ID_DESC",
	R_TRENER_ASC = "R_TRENER_ASC",
	R_TRENER_DESC = "R_TRENER_DESC",
	R_DATUM_ASC = "R_DATUM_ASC",
	R_DATUM_DESC = "R_DATUM_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_R_TRENER__U_ID_ASC = "USER_BY_R_TRENER__U_ID_ASC",
	USER_BY_R_TRENER__U_ID_DESC = "USER_BY_R_TRENER__U_ID_DESC",
	USER_BY_R_TRENER__U_LOGIN_ASC = "USER_BY_R_TRENER__U_LOGIN_ASC",
	USER_BY_R_TRENER__U_LOGIN_DESC = "USER_BY_R_TRENER__U_LOGIN_DESC",
	USER_BY_R_TRENER__U_JMENO_ASC = "USER_BY_R_TRENER__U_JMENO_ASC",
	USER_BY_R_TRENER__U_JMENO_DESC = "USER_BY_R_TRENER__U_JMENO_DESC",
	USER_BY_R_TRENER__U_PRIJMENI_ASC = "USER_BY_R_TRENER__U_PRIJMENI_ASC",
	USER_BY_R_TRENER__U_PRIJMENI_DESC = "USER_BY_R_TRENER__U_PRIJMENI_DESC",
	USER_BY_R_TRENER__U_NAROZENI_ASC = "USER_BY_R_TRENER__U_NAROZENI_ASC",
	USER_BY_R_TRENER__U_NAROZENI_DESC = "USER_BY_R_TRENER__U_NAROZENI_DESC",
	USER_BY_R_TRENER__U_GROUP_ASC = "USER_BY_R_TRENER__U_GROUP_ASC",
	USER_BY_R_TRENER__U_GROUP_DESC = "USER_BY_R_TRENER__U_GROUP_DESC",
	USER_BY_R_TRENER__U_SKUPINA_ASC = "USER_BY_R_TRENER__U_SKUPINA_ASC",
	USER_BY_R_TRENER__U_SKUPINA_DESC = "USER_BY_R_TRENER__U_SKUPINA_DESC",
	USER_BY_R_TRENER__U_BAN_ASC = "USER_BY_R_TRENER__U_BAN_ASC",
	USER_BY_R_TRENER__U_BAN_DESC = "USER_BY_R_TRENER__U_BAN_DESC",
	USER_BY_R_TRENER__U_CONFIRMED_ASC = "USER_BY_R_TRENER__U_CONFIRMED_ASC",
	USER_BY_R_TRENER__U_CONFIRMED_DESC = "USER_BY_R_TRENER__U_CONFIRMED_DESC",
	USER_BY_R_TRENER__U_SYSTEM_ASC = "USER_BY_R_TRENER__U_SYSTEM_ASC",
	USER_BY_R_TRENER__U_SYSTEM_DESC = "USER_BY_R_TRENER__U_SYSTEM_DESC",
	ROZPIS_ITEMS_BY_RI_ID_RODIC__COUNT_ASC = "ROZPIS_ITEMS_BY_RI_ID_RODIC__COUNT_ASC",
	ROZPIS_ITEMS_BY_RI_ID_RODIC__COUNT_DESC = "ROZPIS_ITEMS_BY_RI_ID_RODIC__COUNT_DESC"
}
/** Methods to use when ordering `Session`. */
export const enum SessionsOrderBy {
	NATURAL = "NATURAL",
	SS_ID_ASC = "SS_ID_ASC",
	SS_ID_DESC = "SS_ID_DESC",
	SS_USER_ASC = "SS_USER_ASC",
	SS_USER_DESC = "SS_USER_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_SS_USER__U_ID_ASC = "USER_BY_SS_USER__U_ID_ASC",
	USER_BY_SS_USER__U_ID_DESC = "USER_BY_SS_USER__U_ID_DESC",
	USER_BY_SS_USER__U_LOGIN_ASC = "USER_BY_SS_USER__U_LOGIN_ASC",
	USER_BY_SS_USER__U_LOGIN_DESC = "USER_BY_SS_USER__U_LOGIN_DESC",
	USER_BY_SS_USER__U_JMENO_ASC = "USER_BY_SS_USER__U_JMENO_ASC",
	USER_BY_SS_USER__U_JMENO_DESC = "USER_BY_SS_USER__U_JMENO_DESC",
	USER_BY_SS_USER__U_PRIJMENI_ASC = "USER_BY_SS_USER__U_PRIJMENI_ASC",
	USER_BY_SS_USER__U_PRIJMENI_DESC = "USER_BY_SS_USER__U_PRIJMENI_DESC",
	USER_BY_SS_USER__U_NAROZENI_ASC = "USER_BY_SS_USER__U_NAROZENI_ASC",
	USER_BY_SS_USER__U_NAROZENI_DESC = "USER_BY_SS_USER__U_NAROZENI_DESC",
	USER_BY_SS_USER__U_GROUP_ASC = "USER_BY_SS_USER__U_GROUP_ASC",
	USER_BY_SS_USER__U_GROUP_DESC = "USER_BY_SS_USER__U_GROUP_DESC",
	USER_BY_SS_USER__U_SKUPINA_ASC = "USER_BY_SS_USER__U_SKUPINA_ASC",
	USER_BY_SS_USER__U_SKUPINA_DESC = "USER_BY_SS_USER__U_SKUPINA_DESC",
	USER_BY_SS_USER__U_BAN_ASC = "USER_BY_SS_USER__U_BAN_ASC",
	USER_BY_SS_USER__U_BAN_DESC = "USER_BY_SS_USER__U_BAN_DESC",
	USER_BY_SS_USER__U_CONFIRMED_ASC = "USER_BY_SS_USER__U_CONFIRMED_ASC",
	USER_BY_SS_USER__U_CONFIRMED_DESC = "USER_BY_SS_USER__U_CONFIRMED_DESC",
	USER_BY_SS_USER__U_SYSTEM_ASC = "USER_BY_SS_USER__U_SYSTEM_ASC",
	USER_BY_SS_USER__U_SYSTEM_DESC = "USER_BY_SS_USER__U_SYSTEM_DESC"
}
/** Methods to use when ordering `Upozorneni`. */
export const enum UpozornenisOrderBy {
	NATURAL = "NATURAL",
	UP_ID_ASC = "UP_ID_ASC",
	UP_ID_DESC = "UP_ID_DESC",
	UP_KDO_ASC = "UP_KDO_ASC",
	UP_KDO_DESC = "UP_KDO_DESC",
	UP_TIMESTAMP_ADD_ASC = "UP_TIMESTAMP_ADD_ASC",
	UP_TIMESTAMP_ADD_DESC = "UP_TIMESTAMP_ADD_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_UP_KDO__U_ID_ASC = "USER_BY_UP_KDO__U_ID_ASC",
	USER_BY_UP_KDO__U_ID_DESC = "USER_BY_UP_KDO__U_ID_DESC",
	USER_BY_UP_KDO__U_LOGIN_ASC = "USER_BY_UP_KDO__U_LOGIN_ASC",
	USER_BY_UP_KDO__U_LOGIN_DESC = "USER_BY_UP_KDO__U_LOGIN_DESC",
	USER_BY_UP_KDO__U_JMENO_ASC = "USER_BY_UP_KDO__U_JMENO_ASC",
	USER_BY_UP_KDO__U_JMENO_DESC = "USER_BY_UP_KDO__U_JMENO_DESC",
	USER_BY_UP_KDO__U_PRIJMENI_ASC = "USER_BY_UP_KDO__U_PRIJMENI_ASC",
	USER_BY_UP_KDO__U_PRIJMENI_DESC = "USER_BY_UP_KDO__U_PRIJMENI_DESC",
	USER_BY_UP_KDO__U_NAROZENI_ASC = "USER_BY_UP_KDO__U_NAROZENI_ASC",
	USER_BY_UP_KDO__U_NAROZENI_DESC = "USER_BY_UP_KDO__U_NAROZENI_DESC",
	USER_BY_UP_KDO__U_GROUP_ASC = "USER_BY_UP_KDO__U_GROUP_ASC",
	USER_BY_UP_KDO__U_GROUP_DESC = "USER_BY_UP_KDO__U_GROUP_DESC",
	USER_BY_UP_KDO__U_SKUPINA_ASC = "USER_BY_UP_KDO__U_SKUPINA_ASC",
	USER_BY_UP_KDO__U_SKUPINA_DESC = "USER_BY_UP_KDO__U_SKUPINA_DESC",
	USER_BY_UP_KDO__U_BAN_ASC = "USER_BY_UP_KDO__U_BAN_ASC",
	USER_BY_UP_KDO__U_BAN_DESC = "USER_BY_UP_KDO__U_BAN_DESC",
	USER_BY_UP_KDO__U_CONFIRMED_ASC = "USER_BY_UP_KDO__U_CONFIRMED_ASC",
	USER_BY_UP_KDO__U_CONFIRMED_DESC = "USER_BY_UP_KDO__U_CONFIRMED_DESC",
	USER_BY_UP_KDO__U_SYSTEM_ASC = "USER_BY_UP_KDO__U_SYSTEM_ASC",
	USER_BY_UP_KDO__U_SYSTEM_DESC = "USER_BY_UP_KDO__U_SYSTEM_DESC",
	UPOZORNENI_SKUPINIES_BY_UPS_ID_RODIC__COUNT_ASC = "UPOZORNENI_SKUPINIES_BY_UPS_ID_RODIC__COUNT_ASC",
	UPOZORNENI_SKUPINIES_BY_UPS_ID_RODIC__COUNT_DESC = "UPOZORNENI_SKUPINIES_BY_UPS_ID_RODIC__COUNT_DESC"
}
/** Methods to use when ordering `Attachment`. */
export const enum AttachmentsOrderBy {
	NATURAL = "NATURAL",
	OBJECT_NAME_ASC = "OBJECT_NAME_ASC",
	OBJECT_NAME_DESC = "OBJECT_NAME_DESC",
	UPLOADED_BY_ASC = "UPLOADED_BY_ASC",
	UPLOADED_BY_DESC = "UPLOADED_BY_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USER_BY_UPLOADED_BY__U_ID_ASC = "USER_BY_UPLOADED_BY__U_ID_ASC",
	USER_BY_UPLOADED_BY__U_ID_DESC = "USER_BY_UPLOADED_BY__U_ID_DESC",
	USER_BY_UPLOADED_BY__U_LOGIN_ASC = "USER_BY_UPLOADED_BY__U_LOGIN_ASC",
	USER_BY_UPLOADED_BY__U_LOGIN_DESC = "USER_BY_UPLOADED_BY__U_LOGIN_DESC",
	USER_BY_UPLOADED_BY__U_JMENO_ASC = "USER_BY_UPLOADED_BY__U_JMENO_ASC",
	USER_BY_UPLOADED_BY__U_JMENO_DESC = "USER_BY_UPLOADED_BY__U_JMENO_DESC",
	USER_BY_UPLOADED_BY__U_PRIJMENI_ASC = "USER_BY_UPLOADED_BY__U_PRIJMENI_ASC",
	USER_BY_UPLOADED_BY__U_PRIJMENI_DESC = "USER_BY_UPLOADED_BY__U_PRIJMENI_DESC",
	USER_BY_UPLOADED_BY__U_NAROZENI_ASC = "USER_BY_UPLOADED_BY__U_NAROZENI_ASC",
	USER_BY_UPLOADED_BY__U_NAROZENI_DESC = "USER_BY_UPLOADED_BY__U_NAROZENI_DESC",
	USER_BY_UPLOADED_BY__U_GROUP_ASC = "USER_BY_UPLOADED_BY__U_GROUP_ASC",
	USER_BY_UPLOADED_BY__U_GROUP_DESC = "USER_BY_UPLOADED_BY__U_GROUP_DESC",
	USER_BY_UPLOADED_BY__U_SKUPINA_ASC = "USER_BY_UPLOADED_BY__U_SKUPINA_ASC",
	USER_BY_UPLOADED_BY__U_SKUPINA_DESC = "USER_BY_UPLOADED_BY__U_SKUPINA_DESC",
	USER_BY_UPLOADED_BY__U_BAN_ASC = "USER_BY_UPLOADED_BY__U_BAN_ASC",
	USER_BY_UPLOADED_BY__U_BAN_DESC = "USER_BY_UPLOADED_BY__U_BAN_DESC",
	USER_BY_UPLOADED_BY__U_CONFIRMED_ASC = "USER_BY_UPLOADED_BY__U_CONFIRMED_ASC",
	USER_BY_UPLOADED_BY__U_CONFIRMED_DESC = "USER_BY_UPLOADED_BY__U_CONFIRMED_DESC",
	USER_BY_UPLOADED_BY__U_SYSTEM_ASC = "USER_BY_UPLOADED_BY__U_SYSTEM_ASC",
	USER_BY_UPLOADED_BY__U_SYSTEM_DESC = "USER_BY_UPLOADED_BY__U_SYSTEM_DESC",
	TENANT_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC = "TENANT_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC",
	TENANT_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC = "TENANT_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC",
	LOCATION_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC = "LOCATION_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC",
	LOCATION_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC = "LOCATION_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC",
	ROOM_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC = "ROOM_ATTACHMENTS_BY_OBJECT_NAME__COUNT_ASC",
	ROOM_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC = "ROOM_ATTACHMENTS_BY_OBJECT_NAME__COUNT_DESC"
}
/** Methods to use when ordering `AkceItem`. */
export const enum AkceItemsOrderBy {
	NATURAL = "NATURAL",
	USER_BY_AI_USER__U_ID_ASC = "USER_BY_AI_USER__U_ID_ASC",
	USER_BY_AI_USER__U_ID_DESC = "USER_BY_AI_USER__U_ID_DESC",
	USER_BY_AI_USER__U_LOGIN_ASC = "USER_BY_AI_USER__U_LOGIN_ASC",
	USER_BY_AI_USER__U_LOGIN_DESC = "USER_BY_AI_USER__U_LOGIN_DESC",
	USER_BY_AI_USER__U_JMENO_ASC = "USER_BY_AI_USER__U_JMENO_ASC",
	USER_BY_AI_USER__U_JMENO_DESC = "USER_BY_AI_USER__U_JMENO_DESC",
	USER_BY_AI_USER__U_PRIJMENI_ASC = "USER_BY_AI_USER__U_PRIJMENI_ASC",
	USER_BY_AI_USER__U_PRIJMENI_DESC = "USER_BY_AI_USER__U_PRIJMENI_DESC",
	USER_BY_AI_USER__U_NAROZENI_ASC = "USER_BY_AI_USER__U_NAROZENI_ASC",
	USER_BY_AI_USER__U_NAROZENI_DESC = "USER_BY_AI_USER__U_NAROZENI_DESC",
	USER_BY_AI_USER__U_GROUP_ASC = "USER_BY_AI_USER__U_GROUP_ASC",
	USER_BY_AI_USER__U_GROUP_DESC = "USER_BY_AI_USER__U_GROUP_DESC",
	USER_BY_AI_USER__U_SKUPINA_ASC = "USER_BY_AI_USER__U_SKUPINA_ASC",
	USER_BY_AI_USER__U_SKUPINA_DESC = "USER_BY_AI_USER__U_SKUPINA_DESC",
	USER_BY_AI_USER__U_BAN_ASC = "USER_BY_AI_USER__U_BAN_ASC",
	USER_BY_AI_USER__U_BAN_DESC = "USER_BY_AI_USER__U_BAN_DESC",
	USER_BY_AI_USER__U_CONFIRMED_ASC = "USER_BY_AI_USER__U_CONFIRMED_ASC",
	USER_BY_AI_USER__U_CONFIRMED_DESC = "USER_BY_AI_USER__U_CONFIRMED_DESC",
	USER_BY_AI_USER__U_SYSTEM_ASC = "USER_BY_AI_USER__U_SYSTEM_ASC",
	USER_BY_AI_USER__U_SYSTEM_DESC = "USER_BY_AI_USER__U_SYSTEM_DESC"
}
/** Methods to use when ordering `Akce`. */
export const enum AkcesOrderBy {
	NATURAL = "NATURAL",
	AKCE_ITEMS_BY_AI_ID_RODIC__COUNT_ASC = "AKCE_ITEMS_BY_AI_ID_RODIC__COUNT_ASC",
	AKCE_ITEMS_BY_AI_ID_RODIC__COUNT_DESC = "AKCE_ITEMS_BY_AI_ID_RODIC__COUNT_DESC"
}
/** Methods to use when ordering `Event`. */
export const enum EventsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	SINCE_ASC = "SINCE_ASC",
	SINCE_DESC = "SINCE_DESC",
	IS_VISIBLE_ASC = "IS_VISIBLE_ASC",
	IS_VISIBLE_DESC = "IS_VISIBLE_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	ATTENDEE_USERS_BY_EVENT_ID__COUNT_ASC = "ATTENDEE_USERS_BY_EVENT_ID__COUNT_ASC",
	ATTENDEE_USERS_BY_EVENT_ID__COUNT_DESC = "ATTENDEE_USERS_BY_EVENT_ID__COUNT_DESC",
	ATTENDEE_EXTERNALS_BY_EVENT_ID__COUNT_ASC = "ATTENDEE_EXTERNALS_BY_EVENT_ID__COUNT_ASC",
	ATTENDEE_EXTERNALS_BY_EVENT_ID__COUNT_DESC = "ATTENDEE_EXTERNALS_BY_EVENT_ID__COUNT_DESC"
}
/** Methods to use when ordering `FormResponse`. */
export const enum FormResponsesOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	TYPE_ASC = "TYPE_ASC",
	TYPE_DESC = "TYPE_DESC",
	UPDATED_AT_ASC = "UPDATED_AT_ASC",
	UPDATED_AT_DESC = "UPDATED_AT_DESC",
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
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	GALERIE_FOTOS_BY_GF_ID_RODIC__COUNT_ASC = "GALERIE_FOTOS_BY_GF_ID_RODIC__COUNT_ASC",
	GALERIE_FOTOS_BY_GF_ID_RODIC__COUNT_DESC = "GALERIE_FOTOS_BY_GF_ID_RODIC__COUNT_DESC"
}
/** Methods to use when ordering `Location`. */
export const enum LocationsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	ROOMS_BY_LOCATION__COUNT_ASC = "ROOMS_BY_LOCATION__COUNT_ASC",
	ROOMS_BY_LOCATION__COUNT_DESC = "ROOMS_BY_LOCATION__COUNT_DESC",
	LOCATION_ATTACHMENTS_BY_LOCATION_ID__COUNT_ASC = "LOCATION_ATTACHMENTS_BY_LOCATION_ID__COUNT_ASC",
	LOCATION_ATTACHMENTS_BY_LOCATION_ID__COUNT_DESC = "LOCATION_ATTACHMENTS_BY_LOCATION_ID__COUNT_DESC",
	TENANT_LOCATIONS_BY_LOCATION_ID__COUNT_ASC = "TENANT_LOCATIONS_BY_LOCATION_ID__COUNT_ASC",
	TENANT_LOCATIONS_BY_LOCATION_ID__COUNT_DESC = "TENANT_LOCATIONS_BY_LOCATION_ID__COUNT_DESC"
}
/** Methods to use when ordering `Page`. */
export const enum PagesOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	URL_ASC = "URL_ASC",
	URL_DESC = "URL_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `PageRevision`. */
export const enum PageRevisionsOrderBy {
	NATURAL = "NATURAL",
	REV_NUMBER_ASC = "REV_NUMBER_ASC",
	REV_NUMBER_DESC = "REV_NUMBER_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Parameter`. */
export const enum ParametersOrderBy {
	NATURAL = "NATURAL",
	PA_NAME_ASC = "PA_NAME_ASC",
	PA_NAME_DESC = "PA_NAME_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC"
}
/** Methods to use when ordering `Permission`. */
export const enum PermissionsOrderBy {
	NATURAL = "NATURAL",
	PE_ID_ASC = "PE_ID_ASC",
	PE_ID_DESC = "PE_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	USERS_BY_U_GROUP__COUNT_ASC = "USERS_BY_U_GROUP__COUNT_ASC",
	USERS_BY_U_GROUP__COUNT_DESC = "USERS_BY_U_GROUP__COUNT_DESC"
}
/** Methods to use when ordering `Person`. */
export const enum PeopleOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_PEOPLE_BY_PERSON_ID__COUNT_ASC = "TENANT_PEOPLE_BY_PERSON_ID__COUNT_ASC",
	TENANT_PEOPLE_BY_PERSON_ID__COUNT_DESC = "TENANT_PEOPLE_BY_PERSON_ID__COUNT_DESC"
}
/** Methods to use when ordering `PlatbyCategory`. */
export const enum PlatbyCategoriesOrderBy {
	NATURAL = "NATURAL",
	PC_ID_ASC = "PC_ID_ASC",
	PC_ID_DESC = "PC_ID_DESC",
	PC_SYMBOL_ASC = "PC_SYMBOL_ASC",
	PC_SYMBOL_DESC = "PC_SYMBOL_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	PLATBY_CATEGORY_GROUPS_BY_PCG_ID_CATEGORY__COUNT_ASC = "PLATBY_CATEGORY_GROUPS_BY_PCG_ID_CATEGORY__COUNT_ASC",
	PLATBY_CATEGORY_GROUPS_BY_PCG_ID_CATEGORY__COUNT_DESC = "PLATBY_CATEGORY_GROUPS_BY_PCG_ID_CATEGORY__COUNT_DESC",
	PLATBY_ITEMS_BY_PI_ID_CATEGORY__COUNT_ASC = "PLATBY_ITEMS_BY_PI_ID_CATEGORY__COUNT_ASC",
	PLATBY_ITEMS_BY_PI_ID_CATEGORY__COUNT_DESC = "PLATBY_ITEMS_BY_PI_ID_CATEGORY__COUNT_DESC"
}
/** Methods to use when ordering `PlatbyGroup`. */
export const enum PlatbyGroupsOrderBy {
	NATURAL = "NATURAL",
	PG_ID_ASC = "PG_ID_ASC",
	PG_ID_DESC = "PG_ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	PLATBY_CATEGORY_GROUPS_BY_PCG_ID_GROUP__COUNT_ASC = "PLATBY_CATEGORY_GROUPS_BY_PCG_ID_GROUP__COUNT_ASC",
	PLATBY_CATEGORY_GROUPS_BY_PCG_ID_GROUP__COUNT_DESC = "PLATBY_CATEGORY_GROUPS_BY_PCG_ID_GROUP__COUNT_DESC",
	PLATBY_GROUP_SKUPINAS_BY_PGS_ID_GROUP__COUNT_ASC = "PLATBY_GROUP_SKUPINAS_BY_PGS_ID_GROUP__COUNT_ASC",
	PLATBY_GROUP_SKUPINAS_BY_PGS_ID_GROUP__COUNT_DESC = "PLATBY_GROUP_SKUPINAS_BY_PGS_ID_GROUP__COUNT_DESC"
}
/** Methods to use when ordering `PlatbyRaw`. */
export const enum PlatbyRawsOrderBy {
	NATURAL = "NATURAL",
	PR_ID_ASC = "PR_ID_ASC",
	PR_ID_DESC = "PR_ID_DESC",
	PR_HASH_ASC = "PR_HASH_ASC",
	PR_HASH_DESC = "PR_HASH_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	PLATBY_ITEMS_BY_PI_ID_RAW__COUNT_ASC = "PLATBY_ITEMS_BY_PI_ID_RAW__COUNT_ASC",
	PLATBY_ITEMS_BY_PI_ID_RAW__COUNT_DESC = "PLATBY_ITEMS_BY_PI_ID_RAW__COUNT_DESC"
}
/** Methods to use when ordering `Tenant`. */
export const enum TenantsOrderBy {
	NATURAL = "NATURAL",
	ID_ASC = "ID_ASC",
	ID_DESC = "ID_DESC",
	PRIMARY_KEY_ASC = "PRIMARY_KEY_ASC",
	PRIMARY_KEY_DESC = "PRIMARY_KEY_DESC",
	TENANT_ATTACHMENTS_BY_TENANT_ID__COUNT_ASC = "TENANT_ATTACHMENTS_BY_TENANT_ID__COUNT_ASC",
	TENANT_ATTACHMENTS_BY_TENANT_ID__COUNT_DESC = "TENANT_ATTACHMENTS_BY_TENANT_ID__COUNT_DESC",
	TENANT_PEOPLE_BY_TENANT_ID__COUNT_ASC = "TENANT_PEOPLE_BY_TENANT_ID__COUNT_ASC",
	TENANT_PEOPLE_BY_TENANT_ID__COUNT_DESC = "TENANT_PEOPLE_BY_TENANT_ID__COUNT_DESC",
	COHORT_GROUPS_BY_TENANT__COUNT_ASC = "COHORT_GROUPS_BY_TENANT__COUNT_ASC",
	COHORT_GROUPS_BY_TENANT__COUNT_DESC = "COHORT_GROUPS_BY_TENANT__COUNT_DESC",
	TENANT_LOCATIONS_BY_TENANT_ID__COUNT_ASC = "TENANT_LOCATIONS_BY_TENANT_ID__COUNT_ASC",
	TENANT_LOCATIONS_BY_TENANT_ID__COUNT_DESC = "TENANT_LOCATIONS_BY_TENANT_ID__COUNT_DESC"
}
export const enum CrmCohort {
	DANCER = "DANCER",
	HOBBYIST = "HOBBYIST",
	SHOWDANCE = "SHOWDANCE",
	FREE_LESSON = "FREE_LESSON",
	CONTACT_ME_LATER = "CONTACT_ME_LATER"
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
  

export const Gql = Chain('https://api.rozpisovnik.cz/graphql')