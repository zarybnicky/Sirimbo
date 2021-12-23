/* eslint-disable */
import { ExecutionResult } from 'graphql';
import { TypedDocumentNode as DocumentNode } from '@graphql-typed-document-node/core';
export type Maybe<T> = T | null;
export type InputMaybe<T> = Maybe<T>;
export type Exact<T extends { [key: string]: unknown }> = { [K in keyof T]: T[K] };
export type MakeOptional<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]?: Maybe<T[SubKey]> };
export type MakeMaybe<T, K extends keyof T> = Omit<T, K> & { [SubKey in K]: Maybe<T[SubKey]> };
/** All built-in and custom scalars, mapped to their actual values */
export type Scalars = {
  ID: string;
  String: string;
  Boolean: boolean;
  Int: number;
  Float: number;
  /** A floating point number that requires more precision than IEEE 754 binary 64 */
  BigFloat: any;
  /**
   * A signed eight-byte integer. The upper big integer values are greater than the
   * max value for a JavaScript number. Therefore all big integers will be output as
   * strings and not numbers.
   */
  BigInt: any;
  /** A location in a connection that can be used for resuming pagination. */
  Cursor: any;
  /** The day, does not include a time. */
  Date: any;
  /**
   * A point in time as described by the [ISO
   * 8601](https://en.wikipedia.org/wiki/ISO_8601) standard. May or may not include a timezone.
   */
  Datetime: any;
  /** The `JSON` scalar type represents JSON values as specified by [ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf). */
  JSON: any;
  /** The exact time of day, does not include the date. May or may not have a timezone offset. */
  Time: any;
};

export type Akce = Node & {
  __typename?: 'Akce';
  aDo: Scalars['Date'];
  aDokumenty: Scalars['String'];
  aId: Scalars['BigInt'];
  aInfo: Scalars['String'];
  aJmeno: Scalars['String'];
  aKapacita: Scalars['BigInt'];
  aKde: Scalars['String'];
  aLock: Scalars['Boolean'];
  aOd: Scalars['Date'];
  aTimestamp?: Maybe<Scalars['Datetime']>;
  aVisible: Scalars['Boolean'];
  /** Reads and enables pagination through a set of `AkceItem`. */
  akceItemsByAiIdRodic: AkceItemsConnection;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
};


export type AkceAkceItemsByAiIdRodicArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AkceItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};

/** A condition to be used against `Akce` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type AkceCondition = {
  /** Checks for equality with the object’s `aDo` field. */
  aDo?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `aDokumenty` field. */
  aDokumenty?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `aId` field. */
  aId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `aInfo` field. */
  aInfo?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `aJmeno` field. */
  aJmeno?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `aKapacita` field. */
  aKapacita?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `aKde` field. */
  aKde?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `aLock` field. */
  aLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `aOd` field. */
  aOd?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `aTimestamp` field. */
  aTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `aVisible` field. */
  aVisible?: InputMaybe<Scalars['Boolean']>;
};

/** An input for mutations affecting `Akce` */
export type AkceInput = {
  aDo: Scalars['Date'];
  aDokumenty: Scalars['String'];
  aId?: InputMaybe<Scalars['BigInt']>;
  aInfo: Scalars['String'];
  aJmeno: Scalars['String'];
  aKapacita?: InputMaybe<Scalars['BigInt']>;
  aKde: Scalars['String'];
  aLock?: InputMaybe<Scalars['Boolean']>;
  aOd: Scalars['Date'];
  aTimestamp?: InputMaybe<Scalars['Datetime']>;
  aVisible?: InputMaybe<Scalars['Boolean']>;
};

export type AkceItem = Node & {
  __typename?: 'AkceItem';
  aiId: Scalars['BigInt'];
  aiIdRodic: Scalars['BigInt'];
  aiRokNarozeni: Scalars['Int'];
  aiUser: Scalars['BigInt'];
  /** Reads a single `Akce` that is related to this `AkceItem`. */
  akceByAiIdRodic?: Maybe<Akce>;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `User` that is related to this `AkceItem`. */
  userByAiUser?: Maybe<User>;
};

/**
 * A condition to be used against `AkceItem` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type AkceItemCondition = {
  /** Checks for equality with the object’s `aiId` field. */
  aiId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `aiIdRodic` field. */
  aiIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `aiRokNarozeni` field. */
  aiRokNarozeni?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `aiUser` field. */
  aiUser?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `AkceItem` */
export type AkceItemInput = {
  aiId?: InputMaybe<Scalars['BigInt']>;
  aiIdRodic: Scalars['BigInt'];
  aiRokNarozeni: Scalars['Int'];
  aiUser: Scalars['BigInt'];
};

/** Represents an update to a `AkceItem`. Fields that are set will be updated. */
export type AkceItemPatch = {
  aiId?: InputMaybe<Scalars['BigInt']>;
  aiIdRodic?: InputMaybe<Scalars['BigInt']>;
  aiRokNarozeni?: InputMaybe<Scalars['Int']>;
  aiUser?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `AkceItem` values. */
export type AkceItemsConnection = {
  __typename?: 'AkceItemsConnection';
  /** A list of edges which contains the `AkceItem` and cursor to aid in pagination. */
  edges: Array<AkceItemsEdge>;
  /** A list of `AkceItem` objects. */
  nodes: Array<AkceItem>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `AkceItem` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `AkceItem` edge in the connection. */
export type AkceItemsEdge = {
  __typename?: 'AkceItemsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `AkceItem` at the end of the edge. */
  node: AkceItem;
};

/** Methods to use when ordering `AkceItem`. */
export enum AkceItemsOrderBy {
  AiIdAsc = 'AI_ID_ASC',
  AiIdDesc = 'AI_ID_DESC',
  AiIdRodicAsc = 'AI_ID_RODIC_ASC',
  AiIdRodicDesc = 'AI_ID_RODIC_DESC',
  AiRokNarozeniAsc = 'AI_ROK_NAROZENI_ASC',
  AiRokNarozeniDesc = 'AI_ROK_NAROZENI_DESC',
  AiUserAsc = 'AI_USER_ASC',
  AiUserDesc = 'AI_USER_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** Represents an update to a `Akce`. Fields that are set will be updated. */
export type AkcePatch = {
  aDo?: InputMaybe<Scalars['Date']>;
  aDokumenty?: InputMaybe<Scalars['String']>;
  aId?: InputMaybe<Scalars['BigInt']>;
  aInfo?: InputMaybe<Scalars['String']>;
  aJmeno?: InputMaybe<Scalars['String']>;
  aKapacita?: InputMaybe<Scalars['BigInt']>;
  aKde?: InputMaybe<Scalars['String']>;
  aLock?: InputMaybe<Scalars['Boolean']>;
  aOd?: InputMaybe<Scalars['Date']>;
  aTimestamp?: InputMaybe<Scalars['Datetime']>;
  aVisible?: InputMaybe<Scalars['Boolean']>;
};

/** A connection to a list of `Akce` values. */
export type AkcesConnection = {
  __typename?: 'AkcesConnection';
  /** A list of edges which contains the `Akce` and cursor to aid in pagination. */
  edges: Array<AkcesEdge>;
  /** A list of `Akce` objects. */
  nodes: Array<Akce>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Akce` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Akce` edge in the connection. */
export type AkcesEdge = {
  __typename?: 'AkcesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Akce` at the end of the edge. */
  node: Akce;
};

/** Methods to use when ordering `Akce`. */
export enum AkcesOrderBy {
  ADokumentyAsc = 'A_DOKUMENTY_ASC',
  ADokumentyDesc = 'A_DOKUMENTY_DESC',
  ADoAsc = 'A_DO_ASC',
  ADoDesc = 'A_DO_DESC',
  AIdAsc = 'A_ID_ASC',
  AIdDesc = 'A_ID_DESC',
  AInfoAsc = 'A_INFO_ASC',
  AInfoDesc = 'A_INFO_DESC',
  AJmenoAsc = 'A_JMENO_ASC',
  AJmenoDesc = 'A_JMENO_DESC',
  AKapacitaAsc = 'A_KAPACITA_ASC',
  AKapacitaDesc = 'A_KAPACITA_DESC',
  AKdeAsc = 'A_KDE_ASC',
  AKdeDesc = 'A_KDE_DESC',
  ALockAsc = 'A_LOCK_ASC',
  ALockDesc = 'A_LOCK_DESC',
  AOdAsc = 'A_OD_ASC',
  AOdDesc = 'A_OD_DESC',
  ATimestampAsc = 'A_TIMESTAMP_ASC',
  ATimestampDesc = 'A_TIMESTAMP_DESC',
  AVisibleAsc = 'A_VISIBLE_ASC',
  AVisibleDesc = 'A_VISIBLE_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** A connection to a list of `Aktuality` values. */
export type AktualitiesConnection = {
  __typename?: 'AktualitiesConnection';
  /** A list of edges which contains the `Aktuality` and cursor to aid in pagination. */
  edges: Array<AktualitiesEdge>;
  /** A list of `Aktuality` objects. */
  nodes: Array<Aktuality>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Aktuality` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Aktuality` edge in the connection. */
export type AktualitiesEdge = {
  __typename?: 'AktualitiesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Aktuality` at the end of the edge. */
  node: Aktuality;
};

/** Methods to use when ordering `Aktuality`. */
export enum AktualitiesOrderBy {
  AtFotoAsc = 'AT_FOTO_ASC',
  AtFotoDesc = 'AT_FOTO_DESC',
  AtFotoMainAsc = 'AT_FOTO_MAIN_ASC',
  AtFotoMainDesc = 'AT_FOTO_MAIN_DESC',
  AtIdAsc = 'AT_ID_ASC',
  AtIdDesc = 'AT_ID_DESC',
  AtJmenoAsc = 'AT_JMENO_ASC',
  AtJmenoDesc = 'AT_JMENO_DESC',
  AtKatAsc = 'AT_KAT_ASC',
  AtKatDesc = 'AT_KAT_DESC',
  AtKdoAsc = 'AT_KDO_ASC',
  AtKdoDesc = 'AT_KDO_DESC',
  AtPreviewAsc = 'AT_PREVIEW_ASC',
  AtPreviewDesc = 'AT_PREVIEW_DESC',
  AtTextAsc = 'AT_TEXT_ASC',
  AtTextDesc = 'AT_TEXT_DESC',
  AtTimestampAddAsc = 'AT_TIMESTAMP_ADD_ASC',
  AtTimestampAddDesc = 'AT_TIMESTAMP_ADD_DESC',
  AtTimestampAsc = 'AT_TIMESTAMP_ASC',
  AtTimestampDesc = 'AT_TIMESTAMP_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type Aktuality = Node & {
  __typename?: 'Aktuality';
  atFoto?: Maybe<Scalars['BigInt']>;
  atFotoMain?: Maybe<Scalars['BigInt']>;
  atId: Scalars['BigInt'];
  atJmeno: Scalars['String'];
  atKat: Scalars['String'];
  atKdo: Scalars['BigInt'];
  atPreview: Scalars['String'];
  atText: Scalars['String'];
  atTimestamp?: Maybe<Scalars['Datetime']>;
  atTimestampAdd?: Maybe<Scalars['Datetime']>;
  /** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
  galerieFotoByAtFotoMain?: Maybe<GalerieFoto>;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `User` that is related to this `Aktuality`. */
  userByAtKdo?: Maybe<User>;
};

/**
 * A condition to be used against `Aktuality` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type AktualityCondition = {
  /** Checks for equality with the object’s `atFoto` field. */
  atFoto?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `atFotoMain` field. */
  atFotoMain?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `atId` field. */
  atId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `atJmeno` field. */
  atJmeno?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `atKat` field. */
  atKat?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `atKdo` field. */
  atKdo?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `atPreview` field. */
  atPreview?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `atText` field. */
  atText?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `atTimestamp` field. */
  atTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `atTimestampAdd` field. */
  atTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `Aktuality` */
export type AktualityInput = {
  atFoto?: InputMaybe<Scalars['BigInt']>;
  atFotoMain?: InputMaybe<Scalars['BigInt']>;
  atId?: InputMaybe<Scalars['BigInt']>;
  atJmeno: Scalars['String'];
  atKat: Scalars['String'];
  atKdo: Scalars['BigInt'];
  atPreview: Scalars['String'];
  atText: Scalars['String'];
  atTimestamp?: InputMaybe<Scalars['Datetime']>;
  atTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** Represents an update to a `Aktuality`. Fields that are set will be updated. */
export type AktualityPatch = {
  atFoto?: InputMaybe<Scalars['BigInt']>;
  atFotoMain?: InputMaybe<Scalars['BigInt']>;
  atId?: InputMaybe<Scalars['BigInt']>;
  atJmeno?: InputMaybe<Scalars['String']>;
  atKat?: InputMaybe<Scalars['String']>;
  atKdo?: InputMaybe<Scalars['BigInt']>;
  atPreview?: InputMaybe<Scalars['String']>;
  atText?: InputMaybe<Scalars['String']>;
  atTimestamp?: InputMaybe<Scalars['Datetime']>;
  atTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** All input for the create `Akce` mutation. */
export type CreateAkceInput = {
  /** The `Akce` to be created by this mutation. */
  akce: AkceInput;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the create `AkceItem` mutation. */
export type CreateAkceItemInput = {
  /** The `AkceItem` to be created by this mutation. */
  akceItem: AkceItemInput;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** The output of our create `AkceItem` mutation. */
export type CreateAkceItemPayload = {
  __typename?: 'CreateAkceItemPayload';
  /** Reads a single `Akce` that is related to this `AkceItem`. */
  akceByAiIdRodic?: Maybe<Akce>;
  /** The `AkceItem` that was created by this mutation. */
  akceItem?: Maybe<AkceItem>;
  /** An edge for our `AkceItem`. May be used by Relay 1. */
  akceItemEdge?: Maybe<AkceItemsEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `AkceItem`. */
  userByAiUser?: Maybe<User>;
};


/** The output of our create `AkceItem` mutation. */
export type CreateAkceItemPayloadAkceItemEdgeArgs = {
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};

/** The output of our create `Akce` mutation. */
export type CreateAkcePayload = {
  __typename?: 'CreateAkcePayload';
  /** The `Akce` that was created by this mutation. */
  akce?: Maybe<Akce>;
  /** An edge for our `Akce`. May be used by Relay 1. */
  akceEdge?: Maybe<AkcesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `Akce` mutation. */
export type CreateAkcePayloadAkceEdgeArgs = {
  orderBy?: InputMaybe<Array<AkcesOrderBy>>;
};

/** All input for the create `Aktuality` mutation. */
export type CreateAktualityInput = {
  /** The `Aktuality` to be created by this mutation. */
  aktuality: AktualityInput;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** The output of our create `Aktuality` mutation. */
export type CreateAktualityPayload = {
  __typename?: 'CreateAktualityPayload';
  /** The `Aktuality` that was created by this mutation. */
  aktuality?: Maybe<Aktuality>;
  /** An edge for our `Aktuality`. May be used by Relay 1. */
  aktualityEdge?: Maybe<AktualitiesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
  galerieFotoByAtFotoMain?: Maybe<GalerieFoto>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Aktuality`. */
  userByAtKdo?: Maybe<User>;
};


/** The output of our create `Aktuality` mutation. */
export type CreateAktualityPayloadAktualityEdgeArgs = {
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};

/** All input for the create `Dokumenty` mutation. */
export type CreateDokumentyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Dokumenty` to be created by this mutation. */
  dokumenty: DokumentyInput;
};

/** The output of our create `Dokumenty` mutation. */
export type CreateDokumentyPayload = {
  __typename?: 'CreateDokumentyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Dokumenty` that was created by this mutation. */
  dokumenty?: Maybe<Dokumenty>;
  /** An edge for our `Dokumenty`. May be used by Relay 1. */
  dokumentyEdge?: Maybe<DokumentiesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Dokumenty`. */
  userByDKdo?: Maybe<User>;
};


/** The output of our create `Dokumenty` mutation. */
export type CreateDokumentyPayloadDokumentyEdgeArgs = {
  orderBy?: InputMaybe<Array<DokumentiesOrderBy>>;
};

/** All input for the create `GalerieDir` mutation. */
export type CreateGalerieDirInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `GalerieDir` to be created by this mutation. */
  galerieDir: GalerieDirInput;
};

/** The output of our create `GalerieDir` mutation. */
export type CreateGalerieDirPayload = {
  __typename?: 'CreateGalerieDirPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `GalerieDir` that was created by this mutation. */
  galerieDir?: Maybe<GalerieDir>;
  /** An edge for our `GalerieDir`. May be used by Relay 1. */
  galerieDirEdge?: Maybe<GalerieDirsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `GalerieDir` mutation. */
export type CreateGalerieDirPayloadGalerieDirEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieDirsOrderBy>>;
};

/** All input for the create `GalerieFoto` mutation. */
export type CreateGalerieFotoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `GalerieFoto` to be created by this mutation. */
  galerieFoto: GalerieFotoInput;
};

/** The output of our create `GalerieFoto` mutation. */
export type CreateGalerieFotoPayload = {
  __typename?: 'CreateGalerieFotoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
  galerieDirByGfIdRodic?: Maybe<GalerieDir>;
  /** The `GalerieFoto` that was created by this mutation. */
  galerieFoto?: Maybe<GalerieFoto>;
  /** An edge for our `GalerieFoto`. May be used by Relay 1. */
  galerieFotoEdge?: Maybe<GalerieFotosEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `GalerieFoto`. */
  userByGfKdo?: Maybe<User>;
};


/** The output of our create `GalerieFoto` mutation. */
export type CreateGalerieFotoPayloadGalerieFotoEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};

/** All input for the create `Nabidka` mutation. */
export type CreateNabidkaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Nabidka` to be created by this mutation. */
  nabidka: NabidkaInput;
};

/** All input for the create `NabidkaItem` mutation. */
export type CreateNabidkaItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `NabidkaItem` to be created by this mutation. */
  nabidkaItem: NabidkaItemInput;
};

/** The output of our create `NabidkaItem` mutation. */
export type CreateNabidkaItemPayload = {
  __typename?: 'CreateNabidkaItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
  nabidkaByNiIdRodic?: Maybe<Nabidka>;
  /** The `NabidkaItem` that was created by this mutation. */
  nabidkaItem?: Maybe<NabidkaItem>;
  /** An edge for our `NabidkaItem`. May be used by Relay 1. */
  nabidkaItemEdge?: Maybe<NabidkaItemsEdge>;
  /** Reads a single `Pary` that is related to this `NabidkaItem`. */
  paryByNiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `NabidkaItem` mutation. */
export type CreateNabidkaItemPayloadNabidkaItemEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};

/** The output of our create `Nabidka` mutation. */
export type CreateNabidkaPayload = {
  __typename?: 'CreateNabidkaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Nabidka` that was created by this mutation. */
  nabidka?: Maybe<Nabidka>;
  /** An edge for our `Nabidka`. May be used by Relay 1. */
  nabidkaEdge?: Maybe<NabidkasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Nabidka`. */
  userByNTrener?: Maybe<User>;
};


/** The output of our create `Nabidka` mutation. */
export type CreateNabidkaPayloadNabidkaEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkasOrderBy>>;
};

/** All input for the create `Page` mutation. */
export type CreatePageInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Page` to be created by this mutation. */
  page: PageInput;
};

/** The output of our create `Page` mutation. */
export type CreatePagePayload = {
  __typename?: 'CreatePagePayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Page` that was created by this mutation. */
  page?: Maybe<Page>;
  /** An edge for our `Page`. May be used by Relay 1. */
  pageEdge?: Maybe<PagesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `Page` mutation. */
export type CreatePagePayloadPageEdgeArgs = {
  orderBy?: InputMaybe<Array<PagesOrderBy>>;
};

/** All input for the create `Parameter` mutation. */
export type CreateParameterInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Parameter` to be created by this mutation. */
  parameter: ParameterInput;
};

/** The output of our create `Parameter` mutation. */
export type CreateParameterPayload = {
  __typename?: 'CreateParameterPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Parameter` that was created by this mutation. */
  parameter?: Maybe<Parameter>;
  /** An edge for our `Parameter`. May be used by Relay 1. */
  parameterEdge?: Maybe<ParametersEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `Parameter` mutation. */
export type CreateParameterPayloadParameterEdgeArgs = {
  orderBy?: InputMaybe<Array<ParametersOrderBy>>;
};

/** All input for the create `Pary` mutation. */
export type CreateParyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Pary` to be created by this mutation. */
  pary: ParyInput;
};

/** All input for the create `ParyNavrh` mutation. */
export type CreateParyNavrhInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `ParyNavrh` to be created by this mutation. */
  paryNavrh: ParyNavrhInput;
};

/** The output of our create `ParyNavrh` mutation. */
export type CreateParyNavrhPayload = {
  __typename?: 'CreateParyNavrhPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `ParyNavrh` that was created by this mutation. */
  paryNavrh?: Maybe<ParyNavrh>;
  /** An edge for our `ParyNavrh`. May be used by Relay 1. */
  paryNavrhEdge?: Maybe<ParyNavrhsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnNavrhl?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartner?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartnerka?: Maybe<User>;
};


/** The output of our create `ParyNavrh` mutation. */
export type CreateParyNavrhPayloadParyNavrhEdgeArgs = {
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};

/** The output of our create `Pary` mutation. */
export type CreateParyPayload = {
  __typename?: 'CreateParyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Pary` that was created by this mutation. */
  pary?: Maybe<Pary>;
  /** An edge for our `Pary`. May be used by Relay 1. */
  paryEdge?: Maybe<PariesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Pary`. */
  userByPIdPartner?: Maybe<User>;
};


/** The output of our create `Pary` mutation. */
export type CreateParyPayloadParyEdgeArgs = {
  orderBy?: InputMaybe<Array<PariesOrderBy>>;
};

/** All input for the create `Permission` mutation. */
export type CreatePermissionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Permission` to be created by this mutation. */
  permission: PermissionInput;
};

/** The output of our create `Permission` mutation. */
export type CreatePermissionPayload = {
  __typename?: 'CreatePermissionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Permission` that was created by this mutation. */
  permission?: Maybe<Permission>;
  /** An edge for our `Permission`. May be used by Relay 1. */
  permissionEdge?: Maybe<PermissionsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `Permission` mutation. */
export type CreatePermissionPayloadPermissionEdgeArgs = {
  orderBy?: InputMaybe<Array<PermissionsOrderBy>>;
};

/** All input for the create `PlatbyCategoryGroup` mutation. */
export type CreatePlatbyCategoryGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyCategoryGroup` to be created by this mutation. */
  platbyCategoryGroup: PlatbyCategoryGroupInput;
};

/** The output of our create `PlatbyCategoryGroup` mutation. */
export type CreatePlatbyCategoryGroupPayload = {
  __typename?: 'CreatePlatbyCategoryGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
  platbyCategoryByPcgIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyCategoryGroup` that was created by this mutation. */
  platbyCategoryGroup?: Maybe<PlatbyCategoryGroup>;
  /** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
  platbyCategoryGroupEdge?: Maybe<PlatbyCategoryGroupsEdge>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
  platbyGroupByPcgIdGroup?: Maybe<PlatbyGroup>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `PlatbyCategoryGroup` mutation. */
export type CreatePlatbyCategoryGroupPayloadPlatbyCategoryGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};

/** All input for the create `PlatbyCategory` mutation. */
export type CreatePlatbyCategoryInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyCategory` to be created by this mutation. */
  platbyCategory: PlatbyCategoryInput;
};

/** The output of our create `PlatbyCategory` mutation. */
export type CreatePlatbyCategoryPayload = {
  __typename?: 'CreatePlatbyCategoryPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyCategory` that was created by this mutation. */
  platbyCategory?: Maybe<PlatbyCategory>;
  /** An edge for our `PlatbyCategory`. May be used by Relay 1. */
  platbyCategoryEdge?: Maybe<PlatbyCategoriesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `PlatbyCategory` mutation. */
export type CreatePlatbyCategoryPayloadPlatbyCategoryEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoriesOrderBy>>;
};

/** All input for the create `PlatbyGroup` mutation. */
export type CreatePlatbyGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyGroup` to be created by this mutation. */
  platbyGroup: PlatbyGroupInput;
};

/** The output of our create `PlatbyGroup` mutation. */
export type CreatePlatbyGroupPayload = {
  __typename?: 'CreatePlatbyGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyGroup` that was created by this mutation. */
  platbyGroup?: Maybe<PlatbyGroup>;
  /** An edge for our `PlatbyGroup`. May be used by Relay 1. */
  platbyGroupEdge?: Maybe<PlatbyGroupsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `PlatbyGroup` mutation. */
export type CreatePlatbyGroupPayloadPlatbyGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupsOrderBy>>;
};

/** All input for the create `PlatbyGroupSkupina` mutation. */
export type CreatePlatbyGroupSkupinaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyGroupSkupina` to be created by this mutation. */
  platbyGroupSkupina: PlatbyGroupSkupinaInput;
};

/** The output of our create `PlatbyGroupSkupina` mutation. */
export type CreatePlatbyGroupSkupinaPayload = {
  __typename?: 'CreatePlatbyGroupSkupinaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
  platbyGroupByPgsIdGroup?: Maybe<PlatbyGroup>;
  /** The `PlatbyGroupSkupina` that was created by this mutation. */
  platbyGroupSkupina?: Maybe<PlatbyGroupSkupina>;
  /** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
  platbyGroupSkupinaEdge?: Maybe<PlatbyGroupSkupinasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
  skupinyByPgsIdSkupina?: Maybe<Skupiny>;
};


/** The output of our create `PlatbyGroupSkupina` mutation. */
export type CreatePlatbyGroupSkupinaPayloadPlatbyGroupSkupinaEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};

/** All input for the create `PlatbyItem` mutation. */
export type CreatePlatbyItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyItem` to be created by this mutation. */
  platbyItem: PlatbyItemInput;
};

/** The output of our create `PlatbyItem` mutation. */
export type CreatePlatbyItemPayload = {
  __typename?: 'CreatePlatbyItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
  platbyCategoryByPiIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyItem` that was created by this mutation. */
  platbyItem?: Maybe<PlatbyItem>;
  /** An edge for our `PlatbyItem`. May be used by Relay 1. */
  platbyItemEdge?: Maybe<PlatbyItemsEdge>;
  /** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
  platbyRawByPiIdRaw?: Maybe<PlatbyRaw>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `PlatbyItem`. */
  userByPiIdUser?: Maybe<User>;
};


/** The output of our create `PlatbyItem` mutation. */
export type CreatePlatbyItemPayloadPlatbyItemEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};

/** All input for the create `PlatbyRaw` mutation. */
export type CreatePlatbyRawInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `PlatbyRaw` to be created by this mutation. */
  platbyRaw: PlatbyRawInput;
};

/** The output of our create `PlatbyRaw` mutation. */
export type CreatePlatbyRawPayload = {
  __typename?: 'CreatePlatbyRawPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyRaw` that was created by this mutation. */
  platbyRaw?: Maybe<PlatbyRaw>;
  /** An edge for our `PlatbyRaw`. May be used by Relay 1. */
  platbyRawEdge?: Maybe<PlatbyRawsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our create `PlatbyRaw` mutation. */
export type CreatePlatbyRawPayloadPlatbyRawEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyRawsOrderBy>>;
};

/** All input for the create `Rozpi` mutation. */
export type CreateRozpiInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Rozpi` to be created by this mutation. */
  rozpi: RozpiInput;
};

/** The output of our create `Rozpi` mutation. */
export type CreateRozpiPayload = {
  __typename?: 'CreateRozpiPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Rozpi` that was created by this mutation. */
  rozpi?: Maybe<Rozpi>;
  /** An edge for our `Rozpi`. May be used by Relay 1. */
  rozpiEdge?: Maybe<RozpisEdge>;
  /** Reads a single `User` that is related to this `Rozpi`. */
  userByRTrener?: Maybe<User>;
};


/** The output of our create `Rozpi` mutation. */
export type CreateRozpiPayloadRozpiEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisOrderBy>>;
};

/** All input for the create `RozpisItem` mutation. */
export type CreateRozpisItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `RozpisItem` to be created by this mutation. */
  rozpisItem: RozpisItemInput;
};

/** The output of our create `RozpisItem` mutation. */
export type CreateRozpisItemPayload = {
  __typename?: 'CreateRozpisItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Pary` that is related to this `RozpisItem`. */
  paryByRiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Rozpi` that is related to this `RozpisItem`. */
  rozpiByRiIdRodic?: Maybe<Rozpi>;
  /** The `RozpisItem` that was created by this mutation. */
  rozpisItem?: Maybe<RozpisItem>;
  /** An edge for our `RozpisItem`. May be used by Relay 1. */
  rozpisItemEdge?: Maybe<RozpisItemsEdge>;
};


/** The output of our create `RozpisItem` mutation. */
export type CreateRozpisItemPayloadRozpisItemEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};

/** All input for the create `Session` mutation. */
export type CreateSessionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Session` to be created by this mutation. */
  session: SessionInput;
};

/** The output of our create `Session` mutation. */
export type CreateSessionPayload = {
  __typename?: 'CreateSessionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Session` that was created by this mutation. */
  session?: Maybe<Session>;
  /** An edge for our `Session`. May be used by Relay 1. */
  sessionEdge?: Maybe<SessionsEdge>;
  /** Reads a single `User` that is related to this `Session`. */
  userBySsUser?: Maybe<User>;
};


/** The output of our create `Session` mutation. */
export type CreateSessionPayloadSessionEdgeArgs = {
  orderBy?: InputMaybe<Array<SessionsOrderBy>>;
};

/** All input for the create `Skupiny` mutation. */
export type CreateSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Skupiny` to be created by this mutation. */
  skupiny: SkupinyInput;
};

/** The output of our create `Skupiny` mutation. */
export type CreateSkupinyPayload = {
  __typename?: 'CreateSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Skupiny` that was created by this mutation. */
  skupiny?: Maybe<Skupiny>;
  /** An edge for our `Skupiny`. May be used by Relay 1. */
  skupinyEdge?: Maybe<SkupiniesEdge>;
};


/** The output of our create `Skupiny` mutation. */
export type CreateSkupinyPayloadSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<SkupiniesOrderBy>>;
};

/** All input for the create `Upozorneni` mutation. */
export type CreateUpozorneniInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Upozorneni` to be created by this mutation. */
  upozorneni: UpozorneniInput;
};

/** The output of our create `Upozorneni` mutation. */
export type CreateUpozorneniPayload = {
  __typename?: 'CreateUpozorneniPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Upozorneni` that was created by this mutation. */
  upozorneni?: Maybe<Upozorneni>;
  /** An edge for our `Upozorneni`. May be used by Relay 1. */
  upozorneniEdge?: Maybe<UpozornenisEdge>;
  /** Reads a single `User` that is related to this `Upozorneni`. */
  userByUpKdo?: Maybe<User>;
};


/** The output of our create `Upozorneni` mutation. */
export type CreateUpozorneniPayloadUpozorneniEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozornenisOrderBy>>;
};

/** All input for the create `UpozorneniSkupiny` mutation. */
export type CreateUpozorneniSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `UpozorneniSkupiny` to be created by this mutation. */
  upozorneniSkupiny: UpozorneniSkupinyInput;
};

/** The output of our create `UpozorneniSkupiny` mutation. */
export type CreateUpozorneniSkupinyPayload = {
  __typename?: 'CreateUpozorneniSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
  skupinyByUpsIdSkupina?: Maybe<Skupiny>;
  /** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
  upozorneniByUpsIdRodic?: Maybe<Upozorneni>;
  /** The `UpozorneniSkupiny` that was created by this mutation. */
  upozorneniSkupiny?: Maybe<UpozorneniSkupiny>;
  /** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
  upozorneniSkupinyEdge?: Maybe<UpozorneniSkupiniesEdge>;
};


/** The output of our create `UpozorneniSkupiny` mutation. */
export type CreateUpozorneniSkupinyPayloadUpozorneniSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};

/** All input for the create `User` mutation. */
export type CreateUserInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `User` to be created by this mutation. */
  user: UserInput;
};

/** The output of our create `User` mutation. */
export type CreateUserPayload = {
  __typename?: 'CreateUserPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Permission` that is related to this `User`. */
  permissionByUGroup?: Maybe<Permission>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `User`. */
  skupinyByUSkupina?: Maybe<Skupiny>;
  /** The `User` that was created by this mutation. */
  user?: Maybe<User>;
  /** An edge for our `User`. May be used by Relay 1. */
  userEdge?: Maybe<UsersEdge>;
};


/** The output of our create `User` mutation. */
export type CreateUserPayloadUserEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};

/** All input for the create `UsersSkupiny` mutation. */
export type CreateUsersSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `UsersSkupiny` to be created by this mutation. */
  usersSkupiny: UsersSkupinyInput;
};

/** The output of our create `UsersSkupiny` mutation. */
export type CreateUsersSkupinyPayload = {
  __typename?: 'CreateUsersSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `UsersSkupiny` that was created by this mutation. */
  usersSkupiny?: Maybe<UsersSkupiny>;
  /** An edge for our `UsersSkupiny`. May be used by Relay 1. */
  usersSkupinyEdge?: Maybe<UsersSkupiniesEdge>;
};


/** The output of our create `UsersSkupiny` mutation. */
export type CreateUsersSkupinyPayloadUsersSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersSkupiniesOrderBy>>;
};

/** All input for the create `Video` mutation. */
export type CreateVideoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `Video` to be created by this mutation. */
  video: VideoInput;
};

/** All input for the create `VideoList` mutation. */
export type CreateVideoListInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `VideoList` to be created by this mutation. */
  videoList: VideoListInput;
};

/** The output of our create `VideoList` mutation. */
export type CreateVideoListPayload = {
  __typename?: 'CreateVideoListPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoList` that was created by this mutation. */
  videoList?: Maybe<VideoList>;
  /** An edge for our `VideoList`. May be used by Relay 1. */
  videoListEdge?: Maybe<VideoListsEdge>;
};


/** The output of our create `VideoList` mutation. */
export type CreateVideoListPayloadVideoListEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoListsOrderBy>>;
};

/** The output of our create `Video` mutation. */
export type CreateVideoPayload = {
  __typename?: 'CreateVideoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Video` that was created by this mutation. */
  video?: Maybe<Video>;
  /** An edge for our `Video`. May be used by Relay 1. */
  videoEdge?: Maybe<VideosEdge>;
};


/** The output of our create `Video` mutation. */
export type CreateVideoPayloadVideoEdgeArgs = {
  orderBy?: InputMaybe<Array<VideosOrderBy>>;
};

/** All input for the create `VideoSource` mutation. */
export type CreateVideoSourceInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The `VideoSource` to be created by this mutation. */
  videoSource: VideoSourceInput;
};

/** The output of our create `VideoSource` mutation. */
export type CreateVideoSourcePayload = {
  __typename?: 'CreateVideoSourcePayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoSource` that was created by this mutation. */
  videoSource?: Maybe<VideoSource>;
  /** An edge for our `VideoSource`. May be used by Relay 1. */
  videoSourceEdge?: Maybe<VideoSourcesEdge>;
};


/** The output of our create `VideoSource` mutation. */
export type CreateVideoSourcePayloadVideoSourceEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoSourcesOrderBy>>;
};

/** A `BigInt` edge in the connection. */
export type CurrentCoupleIdEdge = {
  __typename?: 'CurrentCoupleIdEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `BigInt` at the end of the edge. */
  node?: Maybe<Scalars['BigInt']>;
};

/** A connection to a list of `BigInt` values. */
export type CurrentCoupleIdsConnection = {
  __typename?: 'CurrentCoupleIdsConnection';
  /** A list of edges which contains the `BigInt` and cursor to aid in pagination. */
  edges: Array<CurrentCoupleIdEdge>;
  /** A list of `BigInt` objects. */
  nodes: Array<Maybe<Scalars['BigInt']>>;
  /** The count of *all* `BigInt` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** All input for the `deleteAkceByAId` mutation. */
export type DeleteAkceByAIdInput = {
  aId: Scalars['BigInt'];
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `deleteAkce` mutation. */
export type DeleteAkceInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Akce` to be deleted. */
  nodeId: Scalars['ID'];
};

/** All input for the `deleteAkceItemByAiId` mutation. */
export type DeleteAkceItemByAiIdInput = {
  aiId: Scalars['BigInt'];
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `deleteAkceItem` mutation. */
export type DeleteAkceItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `AkceItem` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `AkceItem` mutation. */
export type DeleteAkceItemPayload = {
  __typename?: 'DeleteAkceItemPayload';
  /** Reads a single `Akce` that is related to this `AkceItem`. */
  akceByAiIdRodic?: Maybe<Akce>;
  /** The `AkceItem` that was deleted by this mutation. */
  akceItem?: Maybe<AkceItem>;
  /** An edge for our `AkceItem`. May be used by Relay 1. */
  akceItemEdge?: Maybe<AkceItemsEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedAkceItemId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `AkceItem`. */
  userByAiUser?: Maybe<User>;
};


/** The output of our delete `AkceItem` mutation. */
export type DeleteAkceItemPayloadAkceItemEdgeArgs = {
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};

/** The output of our delete `Akce` mutation. */
export type DeleteAkcePayload = {
  __typename?: 'DeleteAkcePayload';
  /** The `Akce` that was deleted by this mutation. */
  akce?: Maybe<Akce>;
  /** An edge for our `Akce`. May be used by Relay 1. */
  akceEdge?: Maybe<AkcesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedAkceId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `Akce` mutation. */
export type DeleteAkcePayloadAkceEdgeArgs = {
  orderBy?: InputMaybe<Array<AkcesOrderBy>>;
};

/** All input for the `deleteAktualityByAtId` mutation. */
export type DeleteAktualityByAtIdInput = {
  atId: Scalars['BigInt'];
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `deleteAktuality` mutation. */
export type DeleteAktualityInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Aktuality` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Aktuality` mutation. */
export type DeleteAktualityPayload = {
  __typename?: 'DeleteAktualityPayload';
  /** The `Aktuality` that was deleted by this mutation. */
  aktuality?: Maybe<Aktuality>;
  /** An edge for our `Aktuality`. May be used by Relay 1. */
  aktualityEdge?: Maybe<AktualitiesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedAktualityId?: Maybe<Scalars['ID']>;
  /** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
  galerieFotoByAtFotoMain?: Maybe<GalerieFoto>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Aktuality`. */
  userByAtKdo?: Maybe<User>;
};


/** The output of our delete `Aktuality` mutation. */
export type DeleteAktualityPayloadAktualityEdgeArgs = {
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};

/** All input for the `deleteDokumentyByDId` mutation. */
export type DeleteDokumentyByDIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  dId: Scalars['BigInt'];
};

/** All input for the `deleteDokumenty` mutation. */
export type DeleteDokumentyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Dokumenty` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Dokumenty` mutation. */
export type DeleteDokumentyPayload = {
  __typename?: 'DeleteDokumentyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedDokumentyId?: Maybe<Scalars['ID']>;
  /** The `Dokumenty` that was deleted by this mutation. */
  dokumenty?: Maybe<Dokumenty>;
  /** An edge for our `Dokumenty`. May be used by Relay 1. */
  dokumentyEdge?: Maybe<DokumentiesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Dokumenty`. */
  userByDKdo?: Maybe<User>;
};


/** The output of our delete `Dokumenty` mutation. */
export type DeleteDokumentyPayloadDokumentyEdgeArgs = {
  orderBy?: InputMaybe<Array<DokumentiesOrderBy>>;
};

/** All input for the `deleteGalerieDirByGdId` mutation. */
export type DeleteGalerieDirByGdIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  gdId: Scalars['BigInt'];
};

/** All input for the `deleteGalerieDir` mutation. */
export type DeleteGalerieDirInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `GalerieDir` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `GalerieDir` mutation. */
export type DeleteGalerieDirPayload = {
  __typename?: 'DeleteGalerieDirPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedGalerieDirId?: Maybe<Scalars['ID']>;
  /** The `GalerieDir` that was deleted by this mutation. */
  galerieDir?: Maybe<GalerieDir>;
  /** An edge for our `GalerieDir`. May be used by Relay 1. */
  galerieDirEdge?: Maybe<GalerieDirsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `GalerieDir` mutation. */
export type DeleteGalerieDirPayloadGalerieDirEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieDirsOrderBy>>;
};

/** All input for the `deleteGalerieFotoByGfId` mutation. */
export type DeleteGalerieFotoByGfIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  gfId: Scalars['BigInt'];
};

/** All input for the `deleteGalerieFoto` mutation. */
export type DeleteGalerieFotoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `GalerieFoto` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `GalerieFoto` mutation. */
export type DeleteGalerieFotoPayload = {
  __typename?: 'DeleteGalerieFotoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedGalerieFotoId?: Maybe<Scalars['ID']>;
  /** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
  galerieDirByGfIdRodic?: Maybe<GalerieDir>;
  /** The `GalerieFoto` that was deleted by this mutation. */
  galerieFoto?: Maybe<GalerieFoto>;
  /** An edge for our `GalerieFoto`. May be used by Relay 1. */
  galerieFotoEdge?: Maybe<GalerieFotosEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `GalerieFoto`. */
  userByGfKdo?: Maybe<User>;
};


/** The output of our delete `GalerieFoto` mutation. */
export type DeleteGalerieFotoPayloadGalerieFotoEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};

/** All input for the `deleteNabidkaByNId` mutation. */
export type DeleteNabidkaByNIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  nId: Scalars['BigInt'];
};

/** All input for the `deleteNabidka` mutation. */
export type DeleteNabidkaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Nabidka` to be deleted. */
  nodeId: Scalars['ID'];
};

/** All input for the `deleteNabidkaItemByNiId` mutation. */
export type DeleteNabidkaItemByNiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  niId: Scalars['BigInt'];
};

/** All input for the `deleteNabidkaItem` mutation. */
export type DeleteNabidkaItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `NabidkaItem` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `NabidkaItem` mutation. */
export type DeleteNabidkaItemPayload = {
  __typename?: 'DeleteNabidkaItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedNabidkaItemId?: Maybe<Scalars['ID']>;
  /** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
  nabidkaByNiIdRodic?: Maybe<Nabidka>;
  /** The `NabidkaItem` that was deleted by this mutation. */
  nabidkaItem?: Maybe<NabidkaItem>;
  /** An edge for our `NabidkaItem`. May be used by Relay 1. */
  nabidkaItemEdge?: Maybe<NabidkaItemsEdge>;
  /** Reads a single `Pary` that is related to this `NabidkaItem`. */
  paryByNiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `NabidkaItem` mutation. */
export type DeleteNabidkaItemPayloadNabidkaItemEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};

/** The output of our delete `Nabidka` mutation. */
export type DeleteNabidkaPayload = {
  __typename?: 'DeleteNabidkaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedNabidkaId?: Maybe<Scalars['ID']>;
  /** The `Nabidka` that was deleted by this mutation. */
  nabidka?: Maybe<Nabidka>;
  /** An edge for our `Nabidka`. May be used by Relay 1. */
  nabidkaEdge?: Maybe<NabidkasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Nabidka`. */
  userByNTrener?: Maybe<User>;
};


/** The output of our delete `Nabidka` mutation. */
export type DeleteNabidkaPayloadNabidkaEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkasOrderBy>>;
};

/** All input for the `deleteParameterByPaName` mutation. */
export type DeleteParameterByPaNameInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  paName: Scalars['String'];
};

/** All input for the `deleteParameter` mutation. */
export type DeleteParameterInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Parameter` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Parameter` mutation. */
export type DeleteParameterPayload = {
  __typename?: 'DeleteParameterPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedParameterId?: Maybe<Scalars['ID']>;
  /** The `Parameter` that was deleted by this mutation. */
  parameter?: Maybe<Parameter>;
  /** An edge for our `Parameter`. May be used by Relay 1. */
  parameterEdge?: Maybe<ParametersEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `Parameter` mutation. */
export type DeleteParameterPayloadParameterEdgeArgs = {
  orderBy?: InputMaybe<Array<ParametersOrderBy>>;
};

/** All input for the `deleteParyByPId` mutation. */
export type DeleteParyByPIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pId: Scalars['BigInt'];
};

/** All input for the `deletePary` mutation. */
export type DeleteParyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Pary` to be deleted. */
  nodeId: Scalars['ID'];
};

/** All input for the `deleteParyNavrhByPnId` mutation. */
export type DeleteParyNavrhByPnIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pnId: Scalars['BigInt'];
};

/** All input for the `deleteParyNavrh` mutation. */
export type DeleteParyNavrhInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `ParyNavrh` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `ParyNavrh` mutation. */
export type DeleteParyNavrhPayload = {
  __typename?: 'DeleteParyNavrhPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedParyNavrhId?: Maybe<Scalars['ID']>;
  /** The `ParyNavrh` that was deleted by this mutation. */
  paryNavrh?: Maybe<ParyNavrh>;
  /** An edge for our `ParyNavrh`. May be used by Relay 1. */
  paryNavrhEdge?: Maybe<ParyNavrhsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnNavrhl?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartner?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartnerka?: Maybe<User>;
};


/** The output of our delete `ParyNavrh` mutation. */
export type DeleteParyNavrhPayloadParyNavrhEdgeArgs = {
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};

/** The output of our delete `Pary` mutation. */
export type DeleteParyPayload = {
  __typename?: 'DeleteParyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedParyId?: Maybe<Scalars['ID']>;
  /** The `Pary` that was deleted by this mutation. */
  pary?: Maybe<Pary>;
  /** An edge for our `Pary`. May be used by Relay 1. */
  paryEdge?: Maybe<PariesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Pary`. */
  userByPIdPartner?: Maybe<User>;
};


/** The output of our delete `Pary` mutation. */
export type DeleteParyPayloadParyEdgeArgs = {
  orderBy?: InputMaybe<Array<PariesOrderBy>>;
};

/** All input for the `deletePermissionByPeId` mutation. */
export type DeletePermissionByPeIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  peId: Scalars['BigInt'];
};

/** All input for the `deletePermission` mutation. */
export type DeletePermissionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Permission` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Permission` mutation. */
export type DeletePermissionPayload = {
  __typename?: 'DeletePermissionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPermissionId?: Maybe<Scalars['ID']>;
  /** The `Permission` that was deleted by this mutation. */
  permission?: Maybe<Permission>;
  /** An edge for our `Permission`. May be used by Relay 1. */
  permissionEdge?: Maybe<PermissionsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `Permission` mutation. */
export type DeletePermissionPayloadPermissionEdgeArgs = {
  orderBy?: InputMaybe<Array<PermissionsOrderBy>>;
};

/** All input for the `deletePlatbyCategoryByPcId` mutation. */
export type DeletePlatbyCategoryByPcIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pcId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyCategoryGroupByPcgId` mutation. */
export type DeletePlatbyCategoryGroupByPcgIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pcgId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyCategoryGroup` mutation. */
export type DeletePlatbyCategoryGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyCategoryGroup` mutation. */
export type DeletePlatbyCategoryGroupPayload = {
  __typename?: 'DeletePlatbyCategoryGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyCategoryGroupId?: Maybe<Scalars['ID']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
  platbyCategoryByPcgIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyCategoryGroup` that was deleted by this mutation. */
  platbyCategoryGroup?: Maybe<PlatbyCategoryGroup>;
  /** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
  platbyCategoryGroupEdge?: Maybe<PlatbyCategoryGroupsEdge>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
  platbyGroupByPcgIdGroup?: Maybe<PlatbyGroup>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `PlatbyCategoryGroup` mutation. */
export type DeletePlatbyCategoryGroupPayloadPlatbyCategoryGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};

/** All input for the `deletePlatbyCategory` mutation. */
export type DeletePlatbyCategoryInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyCategory` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyCategory` mutation. */
export type DeletePlatbyCategoryPayload = {
  __typename?: 'DeletePlatbyCategoryPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyCategoryId?: Maybe<Scalars['ID']>;
  /** The `PlatbyCategory` that was deleted by this mutation. */
  platbyCategory?: Maybe<PlatbyCategory>;
  /** An edge for our `PlatbyCategory`. May be used by Relay 1. */
  platbyCategoryEdge?: Maybe<PlatbyCategoriesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `PlatbyCategory` mutation. */
export type DeletePlatbyCategoryPayloadPlatbyCategoryEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoriesOrderBy>>;
};

/** All input for the `deletePlatbyGroupByPgId` mutation. */
export type DeletePlatbyGroupByPgIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pgId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyGroup` mutation. */
export type DeletePlatbyGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyGroup` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyGroup` mutation. */
export type DeletePlatbyGroupPayload = {
  __typename?: 'DeletePlatbyGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyGroupId?: Maybe<Scalars['ID']>;
  /** The `PlatbyGroup` that was deleted by this mutation. */
  platbyGroup?: Maybe<PlatbyGroup>;
  /** An edge for our `PlatbyGroup`. May be used by Relay 1. */
  platbyGroupEdge?: Maybe<PlatbyGroupsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `PlatbyGroup` mutation. */
export type DeletePlatbyGroupPayloadPlatbyGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupsOrderBy>>;
};

/** All input for the `deletePlatbyGroupSkupinaByPgsId` mutation. */
export type DeletePlatbyGroupSkupinaByPgsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pgsId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyGroupSkupina` mutation. */
export type DeletePlatbyGroupSkupinaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyGroupSkupina` mutation. */
export type DeletePlatbyGroupSkupinaPayload = {
  __typename?: 'DeletePlatbyGroupSkupinaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyGroupSkupinaId?: Maybe<Scalars['ID']>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
  platbyGroupByPgsIdGroup?: Maybe<PlatbyGroup>;
  /** The `PlatbyGroupSkupina` that was deleted by this mutation. */
  platbyGroupSkupina?: Maybe<PlatbyGroupSkupina>;
  /** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
  platbyGroupSkupinaEdge?: Maybe<PlatbyGroupSkupinasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
  skupinyByPgsIdSkupina?: Maybe<Skupiny>;
};


/** The output of our delete `PlatbyGroupSkupina` mutation. */
export type DeletePlatbyGroupSkupinaPayloadPlatbyGroupSkupinaEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};

/** All input for the `deletePlatbyItemByPiId` mutation. */
export type DeletePlatbyItemByPiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  piId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyItem` mutation. */
export type DeletePlatbyItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyItem` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyItem` mutation. */
export type DeletePlatbyItemPayload = {
  __typename?: 'DeletePlatbyItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyItemId?: Maybe<Scalars['ID']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
  platbyCategoryByPiIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyItem` that was deleted by this mutation. */
  platbyItem?: Maybe<PlatbyItem>;
  /** An edge for our `PlatbyItem`. May be used by Relay 1. */
  platbyItemEdge?: Maybe<PlatbyItemsEdge>;
  /** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
  platbyRawByPiIdRaw?: Maybe<PlatbyRaw>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `PlatbyItem`. */
  userByPiIdUser?: Maybe<User>;
};


/** The output of our delete `PlatbyItem` mutation. */
export type DeletePlatbyItemPayloadPlatbyItemEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};

/** All input for the `deletePlatbyRawByPrId` mutation. */
export type DeletePlatbyRawByPrIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  prId: Scalars['BigInt'];
};

/** All input for the `deletePlatbyRaw` mutation. */
export type DeletePlatbyRawInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyRaw` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `PlatbyRaw` mutation. */
export type DeletePlatbyRawPayload = {
  __typename?: 'DeletePlatbyRawPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedPlatbyRawId?: Maybe<Scalars['ID']>;
  /** The `PlatbyRaw` that was deleted by this mutation. */
  platbyRaw?: Maybe<PlatbyRaw>;
  /** An edge for our `PlatbyRaw`. May be used by Relay 1. */
  platbyRawEdge?: Maybe<PlatbyRawsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our delete `PlatbyRaw` mutation. */
export type DeletePlatbyRawPayloadPlatbyRawEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyRawsOrderBy>>;
};

/** All input for the `deleteRozpiByRId` mutation. */
export type DeleteRozpiByRIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  rId: Scalars['BigInt'];
};

/** All input for the `deleteRozpi` mutation. */
export type DeleteRozpiInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Rozpi` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Rozpi` mutation. */
export type DeleteRozpiPayload = {
  __typename?: 'DeleteRozpiPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedRozpiId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Rozpi` that was deleted by this mutation. */
  rozpi?: Maybe<Rozpi>;
  /** An edge for our `Rozpi`. May be used by Relay 1. */
  rozpiEdge?: Maybe<RozpisEdge>;
  /** Reads a single `User` that is related to this `Rozpi`. */
  userByRTrener?: Maybe<User>;
};


/** The output of our delete `Rozpi` mutation. */
export type DeleteRozpiPayloadRozpiEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisOrderBy>>;
};

/** All input for the `deleteRozpisItemByRiId` mutation. */
export type DeleteRozpisItemByRiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  riId: Scalars['BigInt'];
};

/** All input for the `deleteRozpisItem` mutation. */
export type DeleteRozpisItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `RozpisItem` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `RozpisItem` mutation. */
export type DeleteRozpisItemPayload = {
  __typename?: 'DeleteRozpisItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedRozpisItemId?: Maybe<Scalars['ID']>;
  /** Reads a single `Pary` that is related to this `RozpisItem`. */
  paryByRiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Rozpi` that is related to this `RozpisItem`. */
  rozpiByRiIdRodic?: Maybe<Rozpi>;
  /** The `RozpisItem` that was deleted by this mutation. */
  rozpisItem?: Maybe<RozpisItem>;
  /** An edge for our `RozpisItem`. May be used by Relay 1. */
  rozpisItemEdge?: Maybe<RozpisItemsEdge>;
};


/** The output of our delete `RozpisItem` mutation. */
export type DeleteRozpisItemPayloadRozpisItemEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};

/** All input for the `deleteSessionBySsId` mutation. */
export type DeleteSessionBySsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  ssId: Scalars['String'];
};

/** All input for the `deleteSession` mutation. */
export type DeleteSessionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Session` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Session` mutation. */
export type DeleteSessionPayload = {
  __typename?: 'DeleteSessionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedSessionId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Session` that was deleted by this mutation. */
  session?: Maybe<Session>;
  /** An edge for our `Session`. May be used by Relay 1. */
  sessionEdge?: Maybe<SessionsEdge>;
  /** Reads a single `User` that is related to this `Session`. */
  userBySsUser?: Maybe<User>;
};


/** The output of our delete `Session` mutation. */
export type DeleteSessionPayloadSessionEdgeArgs = {
  orderBy?: InputMaybe<Array<SessionsOrderBy>>;
};

/** All input for the `deleteSkupinyBySId` mutation. */
export type DeleteSkupinyBySIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  sId: Scalars['BigInt'];
};

/** All input for the `deleteSkupiny` mutation. */
export type DeleteSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Skupiny` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Skupiny` mutation. */
export type DeleteSkupinyPayload = {
  __typename?: 'DeleteSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedSkupinyId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Skupiny` that was deleted by this mutation. */
  skupiny?: Maybe<Skupiny>;
  /** An edge for our `Skupiny`. May be used by Relay 1. */
  skupinyEdge?: Maybe<SkupiniesEdge>;
};


/** The output of our delete `Skupiny` mutation. */
export type DeleteSkupinyPayloadSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<SkupiniesOrderBy>>;
};

/** All input for the `deleteUpozorneniByUpId` mutation. */
export type DeleteUpozorneniByUpIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  upId: Scalars['BigInt'];
};

/** All input for the `deleteUpozorneni` mutation. */
export type DeleteUpozorneniInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Upozorneni` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `Upozorneni` mutation. */
export type DeleteUpozorneniPayload = {
  __typename?: 'DeleteUpozorneniPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedUpozorneniId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Upozorneni` that was deleted by this mutation. */
  upozorneni?: Maybe<Upozorneni>;
  /** An edge for our `Upozorneni`. May be used by Relay 1. */
  upozorneniEdge?: Maybe<UpozornenisEdge>;
  /** Reads a single `User` that is related to this `Upozorneni`. */
  userByUpKdo?: Maybe<User>;
};


/** The output of our delete `Upozorneni` mutation. */
export type DeleteUpozorneniPayloadUpozorneniEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozornenisOrderBy>>;
};

/** All input for the `deleteUpozorneniSkupinyByUpsId` mutation. */
export type DeleteUpozorneniSkupinyByUpsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  upsId: Scalars['BigInt'];
};

/** All input for the `deleteUpozorneniSkupiny` mutation. */
export type DeleteUpozorneniSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `UpozorneniSkupiny` mutation. */
export type DeleteUpozorneniSkupinyPayload = {
  __typename?: 'DeleteUpozorneniSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedUpozorneniSkupinyId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
  skupinyByUpsIdSkupina?: Maybe<Skupiny>;
  /** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
  upozorneniByUpsIdRodic?: Maybe<Upozorneni>;
  /** The `UpozorneniSkupiny` that was deleted by this mutation. */
  upozorneniSkupiny?: Maybe<UpozorneniSkupiny>;
  /** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
  upozorneniSkupinyEdge?: Maybe<UpozorneniSkupiniesEdge>;
};


/** The output of our delete `UpozorneniSkupiny` mutation. */
export type DeleteUpozorneniSkupinyPayloadUpozorneniSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};

/** All input for the `deleteUserByUId` mutation. */
export type DeleteUserByUIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  uId: Scalars['BigInt'];
};

/** All input for the `deleteUser` mutation. */
export type DeleteUserInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `User` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `User` mutation. */
export type DeleteUserPayload = {
  __typename?: 'DeleteUserPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedUserId?: Maybe<Scalars['ID']>;
  /** Reads a single `Permission` that is related to this `User`. */
  permissionByUGroup?: Maybe<Permission>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `User`. */
  skupinyByUSkupina?: Maybe<Skupiny>;
  /** The `User` that was deleted by this mutation. */
  user?: Maybe<User>;
  /** An edge for our `User`. May be used by Relay 1. */
  userEdge?: Maybe<UsersEdge>;
};


/** The output of our delete `User` mutation. */
export type DeleteUserPayloadUserEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};

/** All input for the `deleteUsersSkupinyByUsId` mutation. */
export type DeleteUsersSkupinyByUsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  usId: Scalars['BigInt'];
};

/** All input for the `deleteUsersSkupiny` mutation. */
export type DeleteUsersSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `UsersSkupiny` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `UsersSkupiny` mutation. */
export type DeleteUsersSkupinyPayload = {
  __typename?: 'DeleteUsersSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedUsersSkupinyId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `UsersSkupiny` that was deleted by this mutation. */
  usersSkupiny?: Maybe<UsersSkupiny>;
  /** An edge for our `UsersSkupiny`. May be used by Relay 1. */
  usersSkupinyEdge?: Maybe<UsersSkupiniesEdge>;
};


/** The output of our delete `UsersSkupiny` mutation. */
export type DeleteUsersSkupinyPayloadUsersSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersSkupiniesOrderBy>>;
};

/** All input for the `deleteVideoByVId` mutation. */
export type DeleteVideoByVIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  vId: Scalars['BigInt'];
};

/** All input for the `deleteVideo` mutation. */
export type DeleteVideoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Video` to be deleted. */
  nodeId: Scalars['ID'];
};

/** All input for the `deleteVideoListByVlId` mutation. */
export type DeleteVideoListByVlIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  vlId: Scalars['BigInt'];
};

/** All input for the `deleteVideoList` mutation. */
export type DeleteVideoListInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `VideoList` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `VideoList` mutation. */
export type DeleteVideoListPayload = {
  __typename?: 'DeleteVideoListPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedVideoListId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoList` that was deleted by this mutation. */
  videoList?: Maybe<VideoList>;
  /** An edge for our `VideoList`. May be used by Relay 1. */
  videoListEdge?: Maybe<VideoListsEdge>;
};


/** The output of our delete `VideoList` mutation. */
export type DeleteVideoListPayloadVideoListEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoListsOrderBy>>;
};

/** The output of our delete `Video` mutation. */
export type DeleteVideoPayload = {
  __typename?: 'DeleteVideoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedVideoId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Video` that was deleted by this mutation. */
  video?: Maybe<Video>;
  /** An edge for our `Video`. May be used by Relay 1. */
  videoEdge?: Maybe<VideosEdge>;
};


/** The output of our delete `Video` mutation. */
export type DeleteVideoPayloadVideoEdgeArgs = {
  orderBy?: InputMaybe<Array<VideosOrderBy>>;
};

/** All input for the `deleteVideoSourceByVsId` mutation. */
export type DeleteVideoSourceByVsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  vsId: Scalars['BigInt'];
};

/** All input for the `deleteVideoSource` mutation. */
export type DeleteVideoSourceInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `VideoSource` to be deleted. */
  nodeId: Scalars['ID'];
};

/** The output of our delete `VideoSource` mutation. */
export type DeleteVideoSourcePayload = {
  __typename?: 'DeleteVideoSourcePayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  deletedVideoSourceId?: Maybe<Scalars['ID']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoSource` that was deleted by this mutation. */
  videoSource?: Maybe<VideoSource>;
  /** An edge for our `VideoSource`. May be used by Relay 1. */
  videoSourceEdge?: Maybe<VideoSourcesEdge>;
};


/** The output of our delete `VideoSource` mutation. */
export type DeleteVideoSourcePayloadVideoSourceEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoSourcesOrderBy>>;
};

/** A connection to a list of `Dokumenty` values. */
export type DokumentiesConnection = {
  __typename?: 'DokumentiesConnection';
  /** A list of edges which contains the `Dokumenty` and cursor to aid in pagination. */
  edges: Array<DokumentiesEdge>;
  /** A list of `Dokumenty` objects. */
  nodes: Array<Dokumenty>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Dokumenty` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Dokumenty` edge in the connection. */
export type DokumentiesEdge = {
  __typename?: 'DokumentiesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Dokumenty` at the end of the edge. */
  node: Dokumenty;
};

/** Methods to use when ordering `Dokumenty`. */
export enum DokumentiesOrderBy {
  DFilenameAsc = 'D_FILENAME_ASC',
  DFilenameDesc = 'D_FILENAME_DESC',
  DIdAsc = 'D_ID_ASC',
  DIdDesc = 'D_ID_DESC',
  DKategorieAsc = 'D_KATEGORIE_ASC',
  DKategorieDesc = 'D_KATEGORIE_DESC',
  DKdoAsc = 'D_KDO_ASC',
  DKdoDesc = 'D_KDO_DESC',
  DNameAsc = 'D_NAME_ASC',
  DNameDesc = 'D_NAME_DESC',
  DPathAsc = 'D_PATH_ASC',
  DPathDesc = 'D_PATH_DESC',
  DTimestampAsc = 'D_TIMESTAMP_ASC',
  DTimestampDesc = 'D_TIMESTAMP_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type Dokumenty = Node & {
  __typename?: 'Dokumenty';
  dFilename: Scalars['String'];
  dId: Scalars['BigInt'];
  dKategorie: Scalars['Int'];
  dKdo: Scalars['BigInt'];
  dName: Scalars['String'];
  dPath: Scalars['String'];
  dTimestamp?: Maybe<Scalars['Datetime']>;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `User` that is related to this `Dokumenty`. */
  userByDKdo?: Maybe<User>;
};

/**
 * A condition to be used against `Dokumenty` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type DokumentyCondition = {
  /** Checks for equality with the object’s `dFilename` field. */
  dFilename?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `dId` field. */
  dId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `dKategorie` field. */
  dKategorie?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `dKdo` field. */
  dKdo?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `dName` field. */
  dName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `dPath` field. */
  dPath?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `dTimestamp` field. */
  dTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `Dokumenty` */
export type DokumentyInput = {
  dFilename: Scalars['String'];
  dId?: InputMaybe<Scalars['BigInt']>;
  dKategorie: Scalars['Int'];
  dKdo: Scalars['BigInt'];
  dName: Scalars['String'];
  dPath: Scalars['String'];
  dTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** Represents an update to a `Dokumenty`. Fields that are set will be updated. */
export type DokumentyPatch = {
  dFilename?: InputMaybe<Scalars['String']>;
  dId?: InputMaybe<Scalars['BigInt']>;
  dKategorie?: InputMaybe<Scalars['Int']>;
  dKdo?: InputMaybe<Scalars['BigInt']>;
  dName?: InputMaybe<Scalars['String']>;
  dPath?: InputMaybe<Scalars['String']>;
  dTimestamp?: InputMaybe<Scalars['Datetime']>;
};

export type GalerieDir = Node & {
  __typename?: 'GalerieDir';
  /** Reads and enables pagination through a set of `GalerieFoto`. */
  galerieFotosByGfIdRodic: GalerieFotosConnection;
  gdHidden: Scalars['Boolean'];
  gdId: Scalars['BigInt'];
  gdIdRodic: Scalars['BigInt'];
  gdLevel: Scalars['Int'];
  gdName: Scalars['String'];
  gdPath: Scalars['String'];
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
};


export type GalerieDirGalerieFotosByGfIdRodicArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<GalerieFotoCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};

/**
 * A condition to be used against `GalerieDir` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type GalerieDirCondition = {
  /** Checks for equality with the object’s `gdHidden` field. */
  gdHidden?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `gdId` field. */
  gdId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `gdIdRodic` field. */
  gdIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `gdLevel` field. */
  gdLevel?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `gdName` field. */
  gdName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `gdPath` field. */
  gdPath?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `GalerieDir` */
export type GalerieDirInput = {
  gdHidden?: InputMaybe<Scalars['Boolean']>;
  gdId?: InputMaybe<Scalars['BigInt']>;
  gdIdRodic: Scalars['BigInt'];
  gdLevel?: InputMaybe<Scalars['Int']>;
  gdName: Scalars['String'];
  gdPath: Scalars['String'];
};

/** Represents an update to a `GalerieDir`. Fields that are set will be updated. */
export type GalerieDirPatch = {
  gdHidden?: InputMaybe<Scalars['Boolean']>;
  gdId?: InputMaybe<Scalars['BigInt']>;
  gdIdRodic?: InputMaybe<Scalars['BigInt']>;
  gdLevel?: InputMaybe<Scalars['Int']>;
  gdName?: InputMaybe<Scalars['String']>;
  gdPath?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `GalerieDir` values. */
export type GalerieDirsConnection = {
  __typename?: 'GalerieDirsConnection';
  /** A list of edges which contains the `GalerieDir` and cursor to aid in pagination. */
  edges: Array<GalerieDirsEdge>;
  /** A list of `GalerieDir` objects. */
  nodes: Array<GalerieDir>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `GalerieDir` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `GalerieDir` edge in the connection. */
export type GalerieDirsEdge = {
  __typename?: 'GalerieDirsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `GalerieDir` at the end of the edge. */
  node: GalerieDir;
};

/** Methods to use when ordering `GalerieDir`. */
export enum GalerieDirsOrderBy {
  GdHiddenAsc = 'GD_HIDDEN_ASC',
  GdHiddenDesc = 'GD_HIDDEN_DESC',
  GdIdAsc = 'GD_ID_ASC',
  GdIdDesc = 'GD_ID_DESC',
  GdIdRodicAsc = 'GD_ID_RODIC_ASC',
  GdIdRodicDesc = 'GD_ID_RODIC_DESC',
  GdLevelAsc = 'GD_LEVEL_ASC',
  GdLevelDesc = 'GD_LEVEL_DESC',
  GdNameAsc = 'GD_NAME_ASC',
  GdNameDesc = 'GD_NAME_DESC',
  GdPathAsc = 'GD_PATH_ASC',
  GdPathDesc = 'GD_PATH_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type GalerieFoto = Node & {
  __typename?: 'GalerieFoto';
  /** Reads and enables pagination through a set of `Aktuality`. */
  aktualitiesByAtFotoMain: AktualitiesConnection;
  /** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
  galerieDirByGfIdRodic?: Maybe<GalerieDir>;
  gfId: Scalars['BigInt'];
  gfIdRodic: Scalars['BigInt'];
  gfKdo: Scalars['BigInt'];
  gfName: Scalars['String'];
  gfPath: Scalars['String'];
  gfTimestamp?: Maybe<Scalars['Datetime']>;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `User` that is related to this `GalerieFoto`. */
  userByGfKdo?: Maybe<User>;
};


export type GalerieFotoAktualitiesByAtFotoMainArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AktualityCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};

/**
 * A condition to be used against `GalerieFoto` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type GalerieFotoCondition = {
  /** Checks for equality with the object’s `gfId` field. */
  gfId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `gfIdRodic` field. */
  gfIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `gfKdo` field. */
  gfKdo?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `gfName` field. */
  gfName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `gfPath` field. */
  gfPath?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `gfTimestamp` field. */
  gfTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `GalerieFoto` */
export type GalerieFotoInput = {
  gfId?: InputMaybe<Scalars['BigInt']>;
  gfIdRodic: Scalars['BigInt'];
  gfKdo: Scalars['BigInt'];
  gfName: Scalars['String'];
  gfPath: Scalars['String'];
  gfTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** Represents an update to a `GalerieFoto`. Fields that are set will be updated. */
export type GalerieFotoPatch = {
  gfId?: InputMaybe<Scalars['BigInt']>;
  gfIdRodic?: InputMaybe<Scalars['BigInt']>;
  gfKdo?: InputMaybe<Scalars['BigInt']>;
  gfName?: InputMaybe<Scalars['String']>;
  gfPath?: InputMaybe<Scalars['String']>;
  gfTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** A connection to a list of `GalerieFoto` values. */
export type GalerieFotosConnection = {
  __typename?: 'GalerieFotosConnection';
  /** A list of edges which contains the `GalerieFoto` and cursor to aid in pagination. */
  edges: Array<GalerieFotosEdge>;
  /** A list of `GalerieFoto` objects. */
  nodes: Array<GalerieFoto>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `GalerieFoto` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `GalerieFoto` edge in the connection. */
export type GalerieFotosEdge = {
  __typename?: 'GalerieFotosEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `GalerieFoto` at the end of the edge. */
  node: GalerieFoto;
};

/** Methods to use when ordering `GalerieFoto`. */
export enum GalerieFotosOrderBy {
  GfIdAsc = 'GF_ID_ASC',
  GfIdDesc = 'GF_ID_DESC',
  GfIdRodicAsc = 'GF_ID_RODIC_ASC',
  GfIdRodicDesc = 'GF_ID_RODIC_DESC',
  GfKdoAsc = 'GF_KDO_ASC',
  GfKdoDesc = 'GF_KDO_DESC',
  GfNameAsc = 'GF_NAME_ASC',
  GfNameDesc = 'GF_NAME_DESC',
  GfPathAsc = 'GF_PATH_ASC',
  GfPathDesc = 'GF_PATH_DESC',
  GfTimestampAsc = 'GF_TIMESTAMP_ASC',
  GfTimestampDesc = 'GF_TIMESTAMP_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** All input for the `login` mutation. */
export type LoginInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  login: Scalars['String'];
  passwd: Scalars['String'];
};

/** The output of our `login` mutation. */
export type LoginPayload = {
  __typename?: 'LoginPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  string?: Maybe<Scalars['String']>;
};

/** All input for the `logout` mutation. */
export type LogoutInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** The output of our `logout` mutation. */
export type LogoutPayload = {
  __typename?: 'LogoutPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};

export type Member = {
  __typename?: 'Member';
  uEmail?: Maybe<Scalars['String']>;
};

/** A condition to be used against `Member` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type MemberCondition = {
  /** Checks for equality with the object’s `uEmail` field. */
  uEmail?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `Member` values. */
export type MembersConnection = {
  __typename?: 'MembersConnection';
  /** A list of edges which contains the `Member` and cursor to aid in pagination. */
  edges: Array<MembersEdge>;
  /** A list of `Member` objects. */
  nodes: Array<Member>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Member` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Member` edge in the connection. */
export type MembersEdge = {
  __typename?: 'MembersEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Member` at the end of the edge. */
  node: Member;
};

/** Methods to use when ordering `Member`. */
export enum MembersOrderBy {
  Natural = 'NATURAL',
  UEmailAsc = 'U_EMAIL_ASC',
  UEmailDesc = 'U_EMAIL_DESC'
}

/** The root mutation type which contains root level fields which mutate data. */
export type Mutation = {
  __typename?: 'Mutation';
  /** Creates a single `Akce`. */
  createAkce?: Maybe<CreateAkcePayload>;
  /** Creates a single `AkceItem`. */
  createAkceItem?: Maybe<CreateAkceItemPayload>;
  /** Creates a single `Aktuality`. */
  createAktuality?: Maybe<CreateAktualityPayload>;
  /** Creates a single `Dokumenty`. */
  createDokumenty?: Maybe<CreateDokumentyPayload>;
  /** Creates a single `GalerieDir`. */
  createGalerieDir?: Maybe<CreateGalerieDirPayload>;
  /** Creates a single `GalerieFoto`. */
  createGalerieFoto?: Maybe<CreateGalerieFotoPayload>;
  /** Creates a single `Nabidka`. */
  createNabidka?: Maybe<CreateNabidkaPayload>;
  /** Creates a single `NabidkaItem`. */
  createNabidkaItem?: Maybe<CreateNabidkaItemPayload>;
  /** Creates a single `Page`. */
  createPage?: Maybe<CreatePagePayload>;
  /** Creates a single `Parameter`. */
  createParameter?: Maybe<CreateParameterPayload>;
  /** Creates a single `Pary`. */
  createPary?: Maybe<CreateParyPayload>;
  /** Creates a single `ParyNavrh`. */
  createParyNavrh?: Maybe<CreateParyNavrhPayload>;
  /** Creates a single `Permission`. */
  createPermission?: Maybe<CreatePermissionPayload>;
  /** Creates a single `PlatbyCategory`. */
  createPlatbyCategory?: Maybe<CreatePlatbyCategoryPayload>;
  /** Creates a single `PlatbyCategoryGroup`. */
  createPlatbyCategoryGroup?: Maybe<CreatePlatbyCategoryGroupPayload>;
  /** Creates a single `PlatbyGroup`. */
  createPlatbyGroup?: Maybe<CreatePlatbyGroupPayload>;
  /** Creates a single `PlatbyGroupSkupina`. */
  createPlatbyGroupSkupina?: Maybe<CreatePlatbyGroupSkupinaPayload>;
  /** Creates a single `PlatbyItem`. */
  createPlatbyItem?: Maybe<CreatePlatbyItemPayload>;
  /** Creates a single `PlatbyRaw`. */
  createPlatbyRaw?: Maybe<CreatePlatbyRawPayload>;
  /** Creates a single `Rozpi`. */
  createRozpi?: Maybe<CreateRozpiPayload>;
  /** Creates a single `RozpisItem`. */
  createRozpisItem?: Maybe<CreateRozpisItemPayload>;
  /** Creates a single `Session`. */
  createSession?: Maybe<CreateSessionPayload>;
  /** Creates a single `Skupiny`. */
  createSkupiny?: Maybe<CreateSkupinyPayload>;
  /** Creates a single `Upozorneni`. */
  createUpozorneni?: Maybe<CreateUpozorneniPayload>;
  /** Creates a single `UpozorneniSkupiny`. */
  createUpozorneniSkupiny?: Maybe<CreateUpozorneniSkupinyPayload>;
  /** Creates a single `User`. */
  createUser?: Maybe<CreateUserPayload>;
  /** Creates a single `UsersSkupiny`. */
  createUsersSkupiny?: Maybe<CreateUsersSkupinyPayload>;
  /** Creates a single `Video`. */
  createVideo?: Maybe<CreateVideoPayload>;
  /** Creates a single `VideoList`. */
  createVideoList?: Maybe<CreateVideoListPayload>;
  /** Creates a single `VideoSource`. */
  createVideoSource?: Maybe<CreateVideoSourcePayload>;
  /** Deletes a single `Akce` using its globally unique id. */
  deleteAkce?: Maybe<DeleteAkcePayload>;
  /** Deletes a single `Akce` using a unique key. */
  deleteAkceByAId?: Maybe<DeleteAkcePayload>;
  /** Deletes a single `AkceItem` using its globally unique id. */
  deleteAkceItem?: Maybe<DeleteAkceItemPayload>;
  /** Deletes a single `AkceItem` using a unique key. */
  deleteAkceItemByAiId?: Maybe<DeleteAkceItemPayload>;
  /** Deletes a single `Aktuality` using its globally unique id. */
  deleteAktuality?: Maybe<DeleteAktualityPayload>;
  /** Deletes a single `Aktuality` using a unique key. */
  deleteAktualityByAtId?: Maybe<DeleteAktualityPayload>;
  /** Deletes a single `Dokumenty` using its globally unique id. */
  deleteDokumenty?: Maybe<DeleteDokumentyPayload>;
  /** Deletes a single `Dokumenty` using a unique key. */
  deleteDokumentyByDId?: Maybe<DeleteDokumentyPayload>;
  /** Deletes a single `GalerieDir` using its globally unique id. */
  deleteGalerieDir?: Maybe<DeleteGalerieDirPayload>;
  /** Deletes a single `GalerieDir` using a unique key. */
  deleteGalerieDirByGdId?: Maybe<DeleteGalerieDirPayload>;
  /** Deletes a single `GalerieFoto` using its globally unique id. */
  deleteGalerieFoto?: Maybe<DeleteGalerieFotoPayload>;
  /** Deletes a single `GalerieFoto` using a unique key. */
  deleteGalerieFotoByGfId?: Maybe<DeleteGalerieFotoPayload>;
  /** Deletes a single `Nabidka` using its globally unique id. */
  deleteNabidka?: Maybe<DeleteNabidkaPayload>;
  /** Deletes a single `Nabidka` using a unique key. */
  deleteNabidkaByNId?: Maybe<DeleteNabidkaPayload>;
  /** Deletes a single `NabidkaItem` using its globally unique id. */
  deleteNabidkaItem?: Maybe<DeleteNabidkaItemPayload>;
  /** Deletes a single `NabidkaItem` using a unique key. */
  deleteNabidkaItemByNiId?: Maybe<DeleteNabidkaItemPayload>;
  /** Deletes a single `Parameter` using its globally unique id. */
  deleteParameter?: Maybe<DeleteParameterPayload>;
  /** Deletes a single `Parameter` using a unique key. */
  deleteParameterByPaName?: Maybe<DeleteParameterPayload>;
  /** Deletes a single `Pary` using its globally unique id. */
  deletePary?: Maybe<DeleteParyPayload>;
  /** Deletes a single `Pary` using a unique key. */
  deleteParyByPId?: Maybe<DeleteParyPayload>;
  /** Deletes a single `ParyNavrh` using its globally unique id. */
  deleteParyNavrh?: Maybe<DeleteParyNavrhPayload>;
  /** Deletes a single `ParyNavrh` using a unique key. */
  deleteParyNavrhByPnId?: Maybe<DeleteParyNavrhPayload>;
  /** Deletes a single `Permission` using its globally unique id. */
  deletePermission?: Maybe<DeletePermissionPayload>;
  /** Deletes a single `Permission` using a unique key. */
  deletePermissionByPeId?: Maybe<DeletePermissionPayload>;
  /** Deletes a single `PlatbyCategory` using its globally unique id. */
  deletePlatbyCategory?: Maybe<DeletePlatbyCategoryPayload>;
  /** Deletes a single `PlatbyCategory` using a unique key. */
  deletePlatbyCategoryByPcId?: Maybe<DeletePlatbyCategoryPayload>;
  /** Deletes a single `PlatbyCategoryGroup` using its globally unique id. */
  deletePlatbyCategoryGroup?: Maybe<DeletePlatbyCategoryGroupPayload>;
  /** Deletes a single `PlatbyCategoryGroup` using a unique key. */
  deletePlatbyCategoryGroupByPcgId?: Maybe<DeletePlatbyCategoryGroupPayload>;
  /** Deletes a single `PlatbyGroup` using its globally unique id. */
  deletePlatbyGroup?: Maybe<DeletePlatbyGroupPayload>;
  /** Deletes a single `PlatbyGroup` using a unique key. */
  deletePlatbyGroupByPgId?: Maybe<DeletePlatbyGroupPayload>;
  /** Deletes a single `PlatbyGroupSkupina` using its globally unique id. */
  deletePlatbyGroupSkupina?: Maybe<DeletePlatbyGroupSkupinaPayload>;
  /** Deletes a single `PlatbyGroupSkupina` using a unique key. */
  deletePlatbyGroupSkupinaByPgsId?: Maybe<DeletePlatbyGroupSkupinaPayload>;
  /** Deletes a single `PlatbyItem` using its globally unique id. */
  deletePlatbyItem?: Maybe<DeletePlatbyItemPayload>;
  /** Deletes a single `PlatbyItem` using a unique key. */
  deletePlatbyItemByPiId?: Maybe<DeletePlatbyItemPayload>;
  /** Deletes a single `PlatbyRaw` using its globally unique id. */
  deletePlatbyRaw?: Maybe<DeletePlatbyRawPayload>;
  /** Deletes a single `PlatbyRaw` using a unique key. */
  deletePlatbyRawByPrId?: Maybe<DeletePlatbyRawPayload>;
  /** Deletes a single `Rozpi` using its globally unique id. */
  deleteRozpi?: Maybe<DeleteRozpiPayload>;
  /** Deletes a single `Rozpi` using a unique key. */
  deleteRozpiByRId?: Maybe<DeleteRozpiPayload>;
  /** Deletes a single `RozpisItem` using its globally unique id. */
  deleteRozpisItem?: Maybe<DeleteRozpisItemPayload>;
  /** Deletes a single `RozpisItem` using a unique key. */
  deleteRozpisItemByRiId?: Maybe<DeleteRozpisItemPayload>;
  /** Deletes a single `Session` using its globally unique id. */
  deleteSession?: Maybe<DeleteSessionPayload>;
  /** Deletes a single `Session` using a unique key. */
  deleteSessionBySsId?: Maybe<DeleteSessionPayload>;
  /** Deletes a single `Skupiny` using its globally unique id. */
  deleteSkupiny?: Maybe<DeleteSkupinyPayload>;
  /** Deletes a single `Skupiny` using a unique key. */
  deleteSkupinyBySId?: Maybe<DeleteSkupinyPayload>;
  /** Deletes a single `Upozorneni` using its globally unique id. */
  deleteUpozorneni?: Maybe<DeleteUpozorneniPayload>;
  /** Deletes a single `Upozorneni` using a unique key. */
  deleteUpozorneniByUpId?: Maybe<DeleteUpozorneniPayload>;
  /** Deletes a single `UpozorneniSkupiny` using its globally unique id. */
  deleteUpozorneniSkupiny?: Maybe<DeleteUpozorneniSkupinyPayload>;
  /** Deletes a single `UpozorneniSkupiny` using a unique key. */
  deleteUpozorneniSkupinyByUpsId?: Maybe<DeleteUpozorneniSkupinyPayload>;
  /** Deletes a single `User` using its globally unique id. */
  deleteUser?: Maybe<DeleteUserPayload>;
  /** Deletes a single `User` using a unique key. */
  deleteUserByUId?: Maybe<DeleteUserPayload>;
  /** Deletes a single `UsersSkupiny` using its globally unique id. */
  deleteUsersSkupiny?: Maybe<DeleteUsersSkupinyPayload>;
  /** Deletes a single `UsersSkupiny` using a unique key. */
  deleteUsersSkupinyByUsId?: Maybe<DeleteUsersSkupinyPayload>;
  /** Deletes a single `Video` using its globally unique id. */
  deleteVideo?: Maybe<DeleteVideoPayload>;
  /** Deletes a single `Video` using a unique key. */
  deleteVideoByVId?: Maybe<DeleteVideoPayload>;
  /** Deletes a single `VideoList` using its globally unique id. */
  deleteVideoList?: Maybe<DeleteVideoListPayload>;
  /** Deletes a single `VideoList` using a unique key. */
  deleteVideoListByVlId?: Maybe<DeleteVideoListPayload>;
  /** Deletes a single `VideoSource` using its globally unique id. */
  deleteVideoSource?: Maybe<DeleteVideoSourcePayload>;
  /** Deletes a single `VideoSource` using a unique key. */
  deleteVideoSourceByVsId?: Maybe<DeleteVideoSourcePayload>;
  login?: Maybe<LoginPayload>;
  logout?: Maybe<LogoutPayload>;
  /** Updates a single `Akce` using its globally unique id and a patch. */
  updateAkce?: Maybe<UpdateAkcePayload>;
  /** Updates a single `Akce` using a unique key and a patch. */
  updateAkceByAId?: Maybe<UpdateAkcePayload>;
  /** Updates a single `AkceItem` using its globally unique id and a patch. */
  updateAkceItem?: Maybe<UpdateAkceItemPayload>;
  /** Updates a single `AkceItem` using a unique key and a patch. */
  updateAkceItemByAiId?: Maybe<UpdateAkceItemPayload>;
  /** Updates a single `Aktuality` using its globally unique id and a patch. */
  updateAktuality?: Maybe<UpdateAktualityPayload>;
  /** Updates a single `Aktuality` using a unique key and a patch. */
  updateAktualityByAtId?: Maybe<UpdateAktualityPayload>;
  /** Updates a single `Dokumenty` using its globally unique id and a patch. */
  updateDokumenty?: Maybe<UpdateDokumentyPayload>;
  /** Updates a single `Dokumenty` using a unique key and a patch. */
  updateDokumentyByDId?: Maybe<UpdateDokumentyPayload>;
  /** Updates a single `GalerieDir` using its globally unique id and a patch. */
  updateGalerieDir?: Maybe<UpdateGalerieDirPayload>;
  /** Updates a single `GalerieDir` using a unique key and a patch. */
  updateGalerieDirByGdId?: Maybe<UpdateGalerieDirPayload>;
  /** Updates a single `GalerieFoto` using its globally unique id and a patch. */
  updateGalerieFoto?: Maybe<UpdateGalerieFotoPayload>;
  /** Updates a single `GalerieFoto` using a unique key and a patch. */
  updateGalerieFotoByGfId?: Maybe<UpdateGalerieFotoPayload>;
  /** Updates a single `Nabidka` using its globally unique id and a patch. */
  updateNabidka?: Maybe<UpdateNabidkaPayload>;
  /** Updates a single `Nabidka` using a unique key and a patch. */
  updateNabidkaByNId?: Maybe<UpdateNabidkaPayload>;
  /** Updates a single `NabidkaItem` using its globally unique id and a patch. */
  updateNabidkaItem?: Maybe<UpdateNabidkaItemPayload>;
  /** Updates a single `NabidkaItem` using a unique key and a patch. */
  updateNabidkaItemByNiId?: Maybe<UpdateNabidkaItemPayload>;
  /** Updates a single `Page` using its globally unique id and a patch. */
  updatePage?: Maybe<UpdatePagePayload>;
  /** Updates a single `Page` using a unique key and a patch. */
  updatePageById?: Maybe<UpdatePagePayload>;
  /** Updates a single `Page` using a unique key and a patch. */
  updatePageByUrl?: Maybe<UpdatePagePayload>;
  /** Updates a single `Parameter` using its globally unique id and a patch. */
  updateParameter?: Maybe<UpdateParameterPayload>;
  /** Updates a single `Parameter` using a unique key and a patch. */
  updateParameterByPaName?: Maybe<UpdateParameterPayload>;
  /** Updates a single `Pary` using its globally unique id and a patch. */
  updatePary?: Maybe<UpdateParyPayload>;
  /** Updates a single `Pary` using a unique key and a patch. */
  updateParyByPId?: Maybe<UpdateParyPayload>;
  /** Updates a single `ParyNavrh` using its globally unique id and a patch. */
  updateParyNavrh?: Maybe<UpdateParyNavrhPayload>;
  /** Updates a single `ParyNavrh` using a unique key and a patch. */
  updateParyNavrhByPnId?: Maybe<UpdateParyNavrhPayload>;
  /** Updates a single `Permission` using its globally unique id and a patch. */
  updatePermission?: Maybe<UpdatePermissionPayload>;
  /** Updates a single `Permission` using a unique key and a patch. */
  updatePermissionByPeId?: Maybe<UpdatePermissionPayload>;
  /** Updates a single `PlatbyCategory` using its globally unique id and a patch. */
  updatePlatbyCategory?: Maybe<UpdatePlatbyCategoryPayload>;
  /** Updates a single `PlatbyCategory` using a unique key and a patch. */
  updatePlatbyCategoryByPcId?: Maybe<UpdatePlatbyCategoryPayload>;
  /** Updates a single `PlatbyCategoryGroup` using its globally unique id and a patch. */
  updatePlatbyCategoryGroup?: Maybe<UpdatePlatbyCategoryGroupPayload>;
  /** Updates a single `PlatbyCategoryGroup` using a unique key and a patch. */
  updatePlatbyCategoryGroupByPcgId?: Maybe<UpdatePlatbyCategoryGroupPayload>;
  /** Updates a single `PlatbyGroup` using its globally unique id and a patch. */
  updatePlatbyGroup?: Maybe<UpdatePlatbyGroupPayload>;
  /** Updates a single `PlatbyGroup` using a unique key and a patch. */
  updatePlatbyGroupByPgId?: Maybe<UpdatePlatbyGroupPayload>;
  /** Updates a single `PlatbyGroupSkupina` using its globally unique id and a patch. */
  updatePlatbyGroupSkupina?: Maybe<UpdatePlatbyGroupSkupinaPayload>;
  /** Updates a single `PlatbyGroupSkupina` using a unique key and a patch. */
  updatePlatbyGroupSkupinaByPgsId?: Maybe<UpdatePlatbyGroupSkupinaPayload>;
  /** Updates a single `PlatbyItem` using its globally unique id and a patch. */
  updatePlatbyItem?: Maybe<UpdatePlatbyItemPayload>;
  /** Updates a single `PlatbyItem` using a unique key and a patch. */
  updatePlatbyItemByPiId?: Maybe<UpdatePlatbyItemPayload>;
  /** Updates a single `PlatbyRaw` using its globally unique id and a patch. */
  updatePlatbyRaw?: Maybe<UpdatePlatbyRawPayload>;
  /** Updates a single `PlatbyRaw` using a unique key and a patch. */
  updatePlatbyRawByPrId?: Maybe<UpdatePlatbyRawPayload>;
  /** Updates a single `Rozpi` using its globally unique id and a patch. */
  updateRozpi?: Maybe<UpdateRozpiPayload>;
  /** Updates a single `Rozpi` using a unique key and a patch. */
  updateRozpiByRId?: Maybe<UpdateRozpiPayload>;
  /** Updates a single `RozpisItem` using its globally unique id and a patch. */
  updateRozpisItem?: Maybe<UpdateRozpisItemPayload>;
  /** Updates a single `RozpisItem` using a unique key and a patch. */
  updateRozpisItemByRiId?: Maybe<UpdateRozpisItemPayload>;
  /** Updates a single `Session` using its globally unique id and a patch. */
  updateSession?: Maybe<UpdateSessionPayload>;
  /** Updates a single `Session` using a unique key and a patch. */
  updateSessionBySsId?: Maybe<UpdateSessionPayload>;
  /** Updates a single `Skupiny` using its globally unique id and a patch. */
  updateSkupiny?: Maybe<UpdateSkupinyPayload>;
  /** Updates a single `Skupiny` using a unique key and a patch. */
  updateSkupinyBySId?: Maybe<UpdateSkupinyPayload>;
  /** Updates a single `Upozorneni` using its globally unique id and a patch. */
  updateUpozorneni?: Maybe<UpdateUpozorneniPayload>;
  /** Updates a single `Upozorneni` using a unique key and a patch. */
  updateUpozorneniByUpId?: Maybe<UpdateUpozorneniPayload>;
  /** Updates a single `UpozorneniSkupiny` using its globally unique id and a patch. */
  updateUpozorneniSkupiny?: Maybe<UpdateUpozorneniSkupinyPayload>;
  /** Updates a single `UpozorneniSkupiny` using a unique key and a patch. */
  updateUpozorneniSkupinyByUpsId?: Maybe<UpdateUpozorneniSkupinyPayload>;
  /** Updates a single `User` using its globally unique id and a patch. */
  updateUser?: Maybe<UpdateUserPayload>;
  /** Updates a single `User` using a unique key and a patch. */
  updateUserByUId?: Maybe<UpdateUserPayload>;
  /** Updates a single `UsersSkupiny` using its globally unique id and a patch. */
  updateUsersSkupiny?: Maybe<UpdateUsersSkupinyPayload>;
  /** Updates a single `UsersSkupiny` using a unique key and a patch. */
  updateUsersSkupinyByUsId?: Maybe<UpdateUsersSkupinyPayload>;
  /** Updates a single `Video` using its globally unique id and a patch. */
  updateVideo?: Maybe<UpdateVideoPayload>;
  /** Updates a single `Video` using a unique key and a patch. */
  updateVideoByVId?: Maybe<UpdateVideoPayload>;
  /** Updates a single `VideoList` using its globally unique id and a patch. */
  updateVideoList?: Maybe<UpdateVideoListPayload>;
  /** Updates a single `VideoList` using a unique key and a patch. */
  updateVideoListByVlId?: Maybe<UpdateVideoListPayload>;
  /** Updates a single `VideoSource` using its globally unique id and a patch. */
  updateVideoSource?: Maybe<UpdateVideoSourcePayload>;
  /** Updates a single `VideoSource` using a unique key and a patch. */
  updateVideoSourceByVsId?: Maybe<UpdateVideoSourcePayload>;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateAkceArgs = {
  input: CreateAkceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateAkceItemArgs = {
  input: CreateAkceItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateAktualityArgs = {
  input: CreateAktualityInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateDokumentyArgs = {
  input: CreateDokumentyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateGalerieDirArgs = {
  input: CreateGalerieDirInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateGalerieFotoArgs = {
  input: CreateGalerieFotoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateNabidkaArgs = {
  input: CreateNabidkaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateNabidkaItemArgs = {
  input: CreateNabidkaItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePageArgs = {
  input: CreatePageInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateParameterArgs = {
  input: CreateParameterInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateParyArgs = {
  input: CreateParyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateParyNavrhArgs = {
  input: CreateParyNavrhInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePermissionArgs = {
  input: CreatePermissionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyCategoryArgs = {
  input: CreatePlatbyCategoryInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyCategoryGroupArgs = {
  input: CreatePlatbyCategoryGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyGroupArgs = {
  input: CreatePlatbyGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyGroupSkupinaArgs = {
  input: CreatePlatbyGroupSkupinaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyItemArgs = {
  input: CreatePlatbyItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreatePlatbyRawArgs = {
  input: CreatePlatbyRawInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateRozpiArgs = {
  input: CreateRozpiInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateRozpisItemArgs = {
  input: CreateRozpisItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateSessionArgs = {
  input: CreateSessionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateSkupinyArgs = {
  input: CreateSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateUpozorneniArgs = {
  input: CreateUpozorneniInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateUpozorneniSkupinyArgs = {
  input: CreateUpozorneniSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateUserArgs = {
  input: CreateUserInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateUsersSkupinyArgs = {
  input: CreateUsersSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateVideoArgs = {
  input: CreateVideoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateVideoListArgs = {
  input: CreateVideoListInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationCreateVideoSourceArgs = {
  input: CreateVideoSourceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAkceArgs = {
  input: DeleteAkceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAkceByAIdArgs = {
  input: DeleteAkceByAIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAkceItemArgs = {
  input: DeleteAkceItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAkceItemByAiIdArgs = {
  input: DeleteAkceItemByAiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAktualityArgs = {
  input: DeleteAktualityInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteAktualityByAtIdArgs = {
  input: DeleteAktualityByAtIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteDokumentyArgs = {
  input: DeleteDokumentyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteDokumentyByDIdArgs = {
  input: DeleteDokumentyByDIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteGalerieDirArgs = {
  input: DeleteGalerieDirInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteGalerieDirByGdIdArgs = {
  input: DeleteGalerieDirByGdIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteGalerieFotoArgs = {
  input: DeleteGalerieFotoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteGalerieFotoByGfIdArgs = {
  input: DeleteGalerieFotoByGfIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteNabidkaArgs = {
  input: DeleteNabidkaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteNabidkaByNIdArgs = {
  input: DeleteNabidkaByNIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteNabidkaItemArgs = {
  input: DeleteNabidkaItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteNabidkaItemByNiIdArgs = {
  input: DeleteNabidkaItemByNiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParameterArgs = {
  input: DeleteParameterInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParameterByPaNameArgs = {
  input: DeleteParameterByPaNameInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParyArgs = {
  input: DeleteParyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParyByPIdArgs = {
  input: DeleteParyByPIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParyNavrhArgs = {
  input: DeleteParyNavrhInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteParyNavrhByPnIdArgs = {
  input: DeleteParyNavrhByPnIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePermissionArgs = {
  input: DeletePermissionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePermissionByPeIdArgs = {
  input: DeletePermissionByPeIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyCategoryArgs = {
  input: DeletePlatbyCategoryInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyCategoryByPcIdArgs = {
  input: DeletePlatbyCategoryByPcIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyCategoryGroupArgs = {
  input: DeletePlatbyCategoryGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyCategoryGroupByPcgIdArgs = {
  input: DeletePlatbyCategoryGroupByPcgIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyGroupArgs = {
  input: DeletePlatbyGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyGroupByPgIdArgs = {
  input: DeletePlatbyGroupByPgIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyGroupSkupinaArgs = {
  input: DeletePlatbyGroupSkupinaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyGroupSkupinaByPgsIdArgs = {
  input: DeletePlatbyGroupSkupinaByPgsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyItemArgs = {
  input: DeletePlatbyItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyItemByPiIdArgs = {
  input: DeletePlatbyItemByPiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyRawArgs = {
  input: DeletePlatbyRawInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeletePlatbyRawByPrIdArgs = {
  input: DeletePlatbyRawByPrIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteRozpiArgs = {
  input: DeleteRozpiInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteRozpiByRIdArgs = {
  input: DeleteRozpiByRIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteRozpisItemArgs = {
  input: DeleteRozpisItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteRozpisItemByRiIdArgs = {
  input: DeleteRozpisItemByRiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteSessionArgs = {
  input: DeleteSessionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteSessionBySsIdArgs = {
  input: DeleteSessionBySsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteSkupinyArgs = {
  input: DeleteSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteSkupinyBySIdArgs = {
  input: DeleteSkupinyBySIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUpozorneniArgs = {
  input: DeleteUpozorneniInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUpozorneniByUpIdArgs = {
  input: DeleteUpozorneniByUpIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUpozorneniSkupinyArgs = {
  input: DeleteUpozorneniSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUpozorneniSkupinyByUpsIdArgs = {
  input: DeleteUpozorneniSkupinyByUpsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUserArgs = {
  input: DeleteUserInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUserByUIdArgs = {
  input: DeleteUserByUIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUsersSkupinyArgs = {
  input: DeleteUsersSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteUsersSkupinyByUsIdArgs = {
  input: DeleteUsersSkupinyByUsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoArgs = {
  input: DeleteVideoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoByVIdArgs = {
  input: DeleteVideoByVIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoListArgs = {
  input: DeleteVideoListInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoListByVlIdArgs = {
  input: DeleteVideoListByVlIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoSourceArgs = {
  input: DeleteVideoSourceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationDeleteVideoSourceByVsIdArgs = {
  input: DeleteVideoSourceByVsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationLoginArgs = {
  input: LoginInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationLogoutArgs = {
  input: LogoutInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAkceArgs = {
  input: UpdateAkceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAkceByAIdArgs = {
  input: UpdateAkceByAIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAkceItemArgs = {
  input: UpdateAkceItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAkceItemByAiIdArgs = {
  input: UpdateAkceItemByAiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAktualityArgs = {
  input: UpdateAktualityInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateAktualityByAtIdArgs = {
  input: UpdateAktualityByAtIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateDokumentyArgs = {
  input: UpdateDokumentyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateDokumentyByDIdArgs = {
  input: UpdateDokumentyByDIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateGalerieDirArgs = {
  input: UpdateGalerieDirInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateGalerieDirByGdIdArgs = {
  input: UpdateGalerieDirByGdIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateGalerieFotoArgs = {
  input: UpdateGalerieFotoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateGalerieFotoByGfIdArgs = {
  input: UpdateGalerieFotoByGfIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateNabidkaArgs = {
  input: UpdateNabidkaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateNabidkaByNIdArgs = {
  input: UpdateNabidkaByNIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateNabidkaItemArgs = {
  input: UpdateNabidkaItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateNabidkaItemByNiIdArgs = {
  input: UpdateNabidkaItemByNiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePageArgs = {
  input: UpdatePageInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePageByIdArgs = {
  input: UpdatePageByIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePageByUrlArgs = {
  input: UpdatePageByUrlInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParameterArgs = {
  input: UpdateParameterInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParameterByPaNameArgs = {
  input: UpdateParameterByPaNameInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParyArgs = {
  input: UpdateParyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParyByPIdArgs = {
  input: UpdateParyByPIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParyNavrhArgs = {
  input: UpdateParyNavrhInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateParyNavrhByPnIdArgs = {
  input: UpdateParyNavrhByPnIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePermissionArgs = {
  input: UpdatePermissionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePermissionByPeIdArgs = {
  input: UpdatePermissionByPeIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyCategoryArgs = {
  input: UpdatePlatbyCategoryInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyCategoryByPcIdArgs = {
  input: UpdatePlatbyCategoryByPcIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyCategoryGroupArgs = {
  input: UpdatePlatbyCategoryGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyCategoryGroupByPcgIdArgs = {
  input: UpdatePlatbyCategoryGroupByPcgIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyGroupArgs = {
  input: UpdatePlatbyGroupInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyGroupByPgIdArgs = {
  input: UpdatePlatbyGroupByPgIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyGroupSkupinaArgs = {
  input: UpdatePlatbyGroupSkupinaInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyGroupSkupinaByPgsIdArgs = {
  input: UpdatePlatbyGroupSkupinaByPgsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyItemArgs = {
  input: UpdatePlatbyItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyItemByPiIdArgs = {
  input: UpdatePlatbyItemByPiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyRawArgs = {
  input: UpdatePlatbyRawInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdatePlatbyRawByPrIdArgs = {
  input: UpdatePlatbyRawByPrIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateRozpiArgs = {
  input: UpdateRozpiInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateRozpiByRIdArgs = {
  input: UpdateRozpiByRIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateRozpisItemArgs = {
  input: UpdateRozpisItemInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateRozpisItemByRiIdArgs = {
  input: UpdateRozpisItemByRiIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateSessionArgs = {
  input: UpdateSessionInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateSessionBySsIdArgs = {
  input: UpdateSessionBySsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateSkupinyArgs = {
  input: UpdateSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateSkupinyBySIdArgs = {
  input: UpdateSkupinyBySIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUpozorneniArgs = {
  input: UpdateUpozorneniInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUpozorneniByUpIdArgs = {
  input: UpdateUpozorneniByUpIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUpozorneniSkupinyArgs = {
  input: UpdateUpozorneniSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUpozorneniSkupinyByUpsIdArgs = {
  input: UpdateUpozorneniSkupinyByUpsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUserArgs = {
  input: UpdateUserInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUserByUIdArgs = {
  input: UpdateUserByUIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUsersSkupinyArgs = {
  input: UpdateUsersSkupinyInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateUsersSkupinyByUsIdArgs = {
  input: UpdateUsersSkupinyByUsIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoArgs = {
  input: UpdateVideoInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoByVIdArgs = {
  input: UpdateVideoByVIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoListArgs = {
  input: UpdateVideoListInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoListByVlIdArgs = {
  input: UpdateVideoListByVlIdInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoSourceArgs = {
  input: UpdateVideoSourceInput;
};


/** The root mutation type which contains root level fields which mutate data. */
export type MutationUpdateVideoSourceByVsIdArgs = {
  input: UpdateVideoSourceByVsIdInput;
};

export type Nabidka = Node & {
  __typename?: 'Nabidka';
  nDo: Scalars['Date'];
  nId: Scalars['BigInt'];
  nLock: Scalars['Boolean'];
  nMaxPocetHod: Scalars['BigInt'];
  nOd: Scalars['Date'];
  nPocetHod: Scalars['Int'];
  nTimestamp?: Maybe<Scalars['Datetime']>;
  nTrener: Scalars['BigInt'];
  nVisible: Scalars['Boolean'];
  /** Reads and enables pagination through a set of `NabidkaItem`. */
  nabidkaItemsByNiIdRodic: NabidkaItemsConnection;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `User` that is related to this `Nabidka`. */
  userByNTrener?: Maybe<User>;
};


export type NabidkaNabidkaItemsByNiIdRodicArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<NabidkaItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};

/** A condition to be used against `Nabidka` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type NabidkaCondition = {
  /** Checks for equality with the object’s `nDo` field. */
  nDo?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `nId` field. */
  nId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `nLock` field. */
  nLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `nMaxPocetHod` field. */
  nMaxPocetHod?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `nOd` field. */
  nOd?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `nPocetHod` field. */
  nPocetHod?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `nTimestamp` field. */
  nTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `nTrener` field. */
  nTrener?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `nVisible` field. */
  nVisible?: InputMaybe<Scalars['Boolean']>;
};

/** An input for mutations affecting `Nabidka` */
export type NabidkaInput = {
  nDo: Scalars['Date'];
  nId?: InputMaybe<Scalars['BigInt']>;
  nLock?: InputMaybe<Scalars['Boolean']>;
  nMaxPocetHod?: InputMaybe<Scalars['BigInt']>;
  nOd: Scalars['Date'];
  nPocetHod?: InputMaybe<Scalars['Int']>;
  nTimestamp?: InputMaybe<Scalars['Datetime']>;
  nTrener: Scalars['BigInt'];
  nVisible?: InputMaybe<Scalars['Boolean']>;
};

export type NabidkaItem = Node & {
  __typename?: 'NabidkaItem';
  /** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
  nabidkaByNiIdRodic?: Maybe<Nabidka>;
  niId: Scalars['BigInt'];
  niIdRodic: Scalars['BigInt'];
  niLock: Scalars['Boolean'];
  niPartner: Scalars['BigInt'];
  niPocetHod: Scalars['Int'];
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `Pary` that is related to this `NabidkaItem`. */
  paryByNiPartner?: Maybe<Pary>;
};

/**
 * A condition to be used against `NabidkaItem` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type NabidkaItemCondition = {
  /** Checks for equality with the object’s `niId` field. */
  niId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `niIdRodic` field. */
  niIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `niLock` field. */
  niLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `niPartner` field. */
  niPartner?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `niPocetHod` field. */
  niPocetHod?: InputMaybe<Scalars['Int']>;
};

/** An input for mutations affecting `NabidkaItem` */
export type NabidkaItemInput = {
  niId?: InputMaybe<Scalars['BigInt']>;
  niIdRodic: Scalars['BigInt'];
  niLock?: InputMaybe<Scalars['Boolean']>;
  niPartner: Scalars['BigInt'];
  niPocetHod?: InputMaybe<Scalars['Int']>;
};

/** Represents an update to a `NabidkaItem`. Fields that are set will be updated. */
export type NabidkaItemPatch = {
  niId?: InputMaybe<Scalars['BigInt']>;
  niIdRodic?: InputMaybe<Scalars['BigInt']>;
  niLock?: InputMaybe<Scalars['Boolean']>;
  niPartner?: InputMaybe<Scalars['BigInt']>;
  niPocetHod?: InputMaybe<Scalars['Int']>;
};

/** A connection to a list of `NabidkaItem` values. */
export type NabidkaItemsConnection = {
  __typename?: 'NabidkaItemsConnection';
  /** A list of edges which contains the `NabidkaItem` and cursor to aid in pagination. */
  edges: Array<NabidkaItemsEdge>;
  /** A list of `NabidkaItem` objects. */
  nodes: Array<NabidkaItem>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `NabidkaItem` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `NabidkaItem` edge in the connection. */
export type NabidkaItemsEdge = {
  __typename?: 'NabidkaItemsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `NabidkaItem` at the end of the edge. */
  node: NabidkaItem;
};

/** Methods to use when ordering `NabidkaItem`. */
export enum NabidkaItemsOrderBy {
  Natural = 'NATURAL',
  NiIdAsc = 'NI_ID_ASC',
  NiIdDesc = 'NI_ID_DESC',
  NiIdRodicAsc = 'NI_ID_RODIC_ASC',
  NiIdRodicDesc = 'NI_ID_RODIC_DESC',
  NiLockAsc = 'NI_LOCK_ASC',
  NiLockDesc = 'NI_LOCK_DESC',
  NiPartnerAsc = 'NI_PARTNER_ASC',
  NiPartnerDesc = 'NI_PARTNER_DESC',
  NiPocetHodAsc = 'NI_POCET_HOD_ASC',
  NiPocetHodDesc = 'NI_POCET_HOD_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** Represents an update to a `Nabidka`. Fields that are set will be updated. */
export type NabidkaPatch = {
  nDo?: InputMaybe<Scalars['Date']>;
  nId?: InputMaybe<Scalars['BigInt']>;
  nLock?: InputMaybe<Scalars['Boolean']>;
  nMaxPocetHod?: InputMaybe<Scalars['BigInt']>;
  nOd?: InputMaybe<Scalars['Date']>;
  nPocetHod?: InputMaybe<Scalars['Int']>;
  nTimestamp?: InputMaybe<Scalars['Datetime']>;
  nTrener?: InputMaybe<Scalars['BigInt']>;
  nVisible?: InputMaybe<Scalars['Boolean']>;
};

/** A connection to a list of `Nabidka` values. */
export type NabidkasConnection = {
  __typename?: 'NabidkasConnection';
  /** A list of edges which contains the `Nabidka` and cursor to aid in pagination. */
  edges: Array<NabidkasEdge>;
  /** A list of `Nabidka` objects. */
  nodes: Array<Nabidka>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Nabidka` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Nabidka` edge in the connection. */
export type NabidkasEdge = {
  __typename?: 'NabidkasEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Nabidka` at the end of the edge. */
  node: Nabidka;
};

/** Methods to use when ordering `Nabidka`. */
export enum NabidkasOrderBy {
  Natural = 'NATURAL',
  NDoAsc = 'N_DO_ASC',
  NDoDesc = 'N_DO_DESC',
  NIdAsc = 'N_ID_ASC',
  NIdDesc = 'N_ID_DESC',
  NLockAsc = 'N_LOCK_ASC',
  NLockDesc = 'N_LOCK_DESC',
  NMaxPocetHodAsc = 'N_MAX_POCET_HOD_ASC',
  NMaxPocetHodDesc = 'N_MAX_POCET_HOD_DESC',
  NOdAsc = 'N_OD_ASC',
  NOdDesc = 'N_OD_DESC',
  NPocetHodAsc = 'N_POCET_HOD_ASC',
  NPocetHodDesc = 'N_POCET_HOD_DESC',
  NTimestampAsc = 'N_TIMESTAMP_ASC',
  NTimestampDesc = 'N_TIMESTAMP_DESC',
  NTrenerAsc = 'N_TRENER_ASC',
  NTrenerDesc = 'N_TRENER_DESC',
  NVisibleAsc = 'N_VISIBLE_ASC',
  NVisibleDesc = 'N_VISIBLE_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** An object with a globally unique `ID`. */
export type Node = {
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
};

export type Page = Node & {
  __typename?: 'Page';
  content: Scalars['JSON'];
  createdAt: Scalars['Datetime'];
  id: Scalars['Int'];
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  updatedAt: Scalars['Datetime'];
  url: Scalars['String'];
};

/** A condition to be used against `Page` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type PageCondition = {
  /** Checks for equality with the object’s `content` field. */
  content?: InputMaybe<Scalars['JSON']>;
  /** Checks for equality with the object’s `createdAt` field. */
  createdAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `id` field. */
  id?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `updatedAt` field. */
  updatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `url` field. */
  url?: InputMaybe<Scalars['String']>;
};

/** Information about pagination in a connection. */
export type PageInfo = {
  __typename?: 'PageInfo';
  /** When paginating forwards, the cursor to continue. */
  endCursor?: Maybe<Scalars['Cursor']>;
  /** When paginating forwards, are there more items? */
  hasNextPage: Scalars['Boolean'];
  /** When paginating backwards, are there more items? */
  hasPreviousPage: Scalars['Boolean'];
  /** When paginating backwards, the cursor to continue. */
  startCursor?: Maybe<Scalars['Cursor']>;
};

/** An input for mutations affecting `Page` */
export type PageInput = {
  content: Scalars['JSON'];
  createdAt?: InputMaybe<Scalars['Datetime']>;
  id?: InputMaybe<Scalars['Int']>;
  updatedAt?: InputMaybe<Scalars['Datetime']>;
  url: Scalars['String'];
};

/** Represents an update to a `Page`. Fields that are set will be updated. */
export type PagePatch = {
  content?: InputMaybe<Scalars['JSON']>;
  createdAt?: InputMaybe<Scalars['Datetime']>;
  id?: InputMaybe<Scalars['Int']>;
  updatedAt?: InputMaybe<Scalars['Datetime']>;
  url?: InputMaybe<Scalars['String']>;
};

export type PageRevision = Node & {
  __typename?: 'PageRevision';
  content: Scalars['JSON'];
  createdAt: Scalars['Datetime'];
  id: Scalars['Int'];
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  revNumber: Scalars['Int'];
  revOperation: Scalars['String'];
  revTimestamp?: Maybe<Scalars['Datetime']>;
  updatedAt: Scalars['Datetime'];
  url: Scalars['String'];
};

/**
 * A condition to be used against `PageRevision` object types. All fields are
 * tested for equality and combined with a logical ‘and.’
 */
export type PageRevisionCondition = {
  /** Checks for equality with the object’s `content` field. */
  content?: InputMaybe<Scalars['JSON']>;
  /** Checks for equality with the object’s `createdAt` field. */
  createdAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `id` field. */
  id?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `revNumber` field. */
  revNumber?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `revOperation` field. */
  revOperation?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `revTimestamp` field. */
  revTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `updatedAt` field. */
  updatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `url` field. */
  url?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `PageRevision` values. */
export type PageRevisionsConnection = {
  __typename?: 'PageRevisionsConnection';
  /** A list of edges which contains the `PageRevision` and cursor to aid in pagination. */
  edges: Array<PageRevisionsEdge>;
  /** A list of `PageRevision` objects. */
  nodes: Array<PageRevision>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PageRevision` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PageRevision` edge in the connection. */
export type PageRevisionsEdge = {
  __typename?: 'PageRevisionsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PageRevision` at the end of the edge. */
  node: PageRevision;
};

/** Methods to use when ordering `PageRevision`. */
export enum PageRevisionsOrderBy {
  ContentAsc = 'CONTENT_ASC',
  ContentDesc = 'CONTENT_DESC',
  CreatedAtAsc = 'CREATED_AT_ASC',
  CreatedAtDesc = 'CREATED_AT_DESC',
  IdAsc = 'ID_ASC',
  IdDesc = 'ID_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  RevNumberAsc = 'REV_NUMBER_ASC',
  RevNumberDesc = 'REV_NUMBER_DESC',
  RevOperationAsc = 'REV_OPERATION_ASC',
  RevOperationDesc = 'REV_OPERATION_DESC',
  RevTimestampAsc = 'REV_TIMESTAMP_ASC',
  RevTimestampDesc = 'REV_TIMESTAMP_DESC',
  UpdatedAtAsc = 'UPDATED_AT_ASC',
  UpdatedAtDesc = 'UPDATED_AT_DESC',
  UrlAsc = 'URL_ASC',
  UrlDesc = 'URL_DESC'
}

/** A connection to a list of `Page` values. */
export type PagesConnection = {
  __typename?: 'PagesConnection';
  /** A list of edges which contains the `Page` and cursor to aid in pagination. */
  edges: Array<PagesEdge>;
  /** A list of `Page` objects. */
  nodes: Array<Page>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Page` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Page` edge in the connection. */
export type PagesEdge = {
  __typename?: 'PagesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Page` at the end of the edge. */
  node: Page;
};

/** Methods to use when ordering `Page`. */
export enum PagesOrderBy {
  ContentAsc = 'CONTENT_ASC',
  ContentDesc = 'CONTENT_DESC',
  CreatedAtAsc = 'CREATED_AT_ASC',
  CreatedAtDesc = 'CREATED_AT_DESC',
  IdAsc = 'ID_ASC',
  IdDesc = 'ID_DESC',
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  UpdatedAtAsc = 'UPDATED_AT_ASC',
  UpdatedAtDesc = 'UPDATED_AT_DESC',
  UrlAsc = 'URL_ASC',
  UrlDesc = 'URL_DESC'
}

export type Parameter = Node & {
  __typename?: 'Parameter';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  paName: Scalars['String'];
  paValue: Scalars['String'];
};

/**
 * A condition to be used against `Parameter` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type ParameterCondition = {
  /** Checks for equality with the object’s `paName` field. */
  paName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `paValue` field. */
  paValue?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `Parameter` */
export type ParameterInput = {
  paName: Scalars['String'];
  paValue: Scalars['String'];
};

/** Represents an update to a `Parameter`. Fields that are set will be updated. */
export type ParameterPatch = {
  paName?: InputMaybe<Scalars['String']>;
  paValue?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `Parameter` values. */
export type ParametersConnection = {
  __typename?: 'ParametersConnection';
  /** A list of edges which contains the `Parameter` and cursor to aid in pagination. */
  edges: Array<ParametersEdge>;
  /** A list of `Parameter` objects. */
  nodes: Array<Parameter>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Parameter` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Parameter` edge in the connection. */
export type ParametersEdge = {
  __typename?: 'ParametersEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Parameter` at the end of the edge. */
  node: Parameter;
};

/** Methods to use when ordering `Parameter`. */
export enum ParametersOrderBy {
  Natural = 'NATURAL',
  PaNameAsc = 'PA_NAME_ASC',
  PaNameDesc = 'PA_NAME_DESC',
  PaValueAsc = 'PA_VALUE_ASC',
  PaValueDesc = 'PA_VALUE_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** A connection to a list of `Pary` values. */
export type PariesConnection = {
  __typename?: 'PariesConnection';
  /** A list of edges which contains the `Pary` and cursor to aid in pagination. */
  edges: Array<PariesEdge>;
  /** A list of `Pary` objects. */
  nodes: Array<Pary>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Pary` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Pary` edge in the connection. */
export type PariesEdge = {
  __typename?: 'PariesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Pary` at the end of the edge. */
  node: Pary;
};

/** Methods to use when ordering `Pary`. */
export enum PariesOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  PArchivAsc = 'P_ARCHIV_ASC',
  PArchivDesc = 'P_ARCHIV_DESC',
  PHodnoceniAsc = 'P_HODNOCENI_ASC',
  PHodnoceniDesc = 'P_HODNOCENI_DESC',
  PIdAsc = 'P_ID_ASC',
  PIdDesc = 'P_ID_DESC',
  PIdPartnerkaAsc = 'P_ID_PARTNERKA_ASC',
  PIdPartnerkaDesc = 'P_ID_PARTNERKA_DESC',
  PIdPartnerAsc = 'P_ID_PARTNER_ASC',
  PIdPartnerDesc = 'P_ID_PARTNER_DESC',
  PLatBodyAsc = 'P_LAT_BODY_ASC',
  PLatBodyDesc = 'P_LAT_BODY_DESC',
  PLatFinaleAsc = 'P_LAT_FINALE_ASC',
  PLatFinaleDesc = 'P_LAT_FINALE_DESC',
  PLatTridaAsc = 'P_LAT_TRIDA_ASC',
  PLatTridaDesc = 'P_LAT_TRIDA_DESC',
  PSttBodyAsc = 'P_STT_BODY_ASC',
  PSttBodyDesc = 'P_STT_BODY_DESC',
  PSttFinaleAsc = 'P_STT_FINALE_ASC',
  PSttFinaleDesc = 'P_STT_FINALE_DESC',
  PSttTridaAsc = 'P_STT_TRIDA_ASC',
  PSttTridaDesc = 'P_STT_TRIDA_DESC',
  PTimestampAddAsc = 'P_TIMESTAMP_ADD_ASC',
  PTimestampAddDesc = 'P_TIMESTAMP_ADD_DESC',
  PTimestampArchiveAsc = 'P_TIMESTAMP_ARCHIVE_ASC',
  PTimestampArchiveDesc = 'P_TIMESTAMP_ARCHIVE_DESC'
}

export type Pary = Node & {
  __typename?: 'Pary';
  /** Reads and enables pagination through a set of `NabidkaItem`. */
  nabidkaItemsByNiPartner: NabidkaItemsConnection;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pArchiv: Scalars['Boolean'];
  pHodnoceni: Scalars['Int'];
  pId: Scalars['BigInt'];
  pIdPartner: Scalars['BigInt'];
  pIdPartnerka?: Maybe<Scalars['BigInt']>;
  pLatBody: Scalars['Int'];
  pLatFinale: Scalars['Boolean'];
  pLatTrida: ParyPLatTrida;
  pSttBody: Scalars['Int'];
  pSttFinale: Scalars['Boolean'];
  pSttTrida: ParyPSttTrida;
  pTimestampAdd: Scalars['Datetime'];
  pTimestampArchive?: Maybe<Scalars['Datetime']>;
  /** Reads and enables pagination through a set of `RozpisItem`. */
  rozpisItemsByRiPartner: RozpisItemsConnection;
  /** Reads a single `User` that is related to this `Pary`. */
  userByPIdPartner?: Maybe<User>;
};


export type ParyNabidkaItemsByNiPartnerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<NabidkaItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};


export type ParyRozpisItemsByRiPartnerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<RozpisItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};

/** A condition to be used against `Pary` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type ParyCondition = {
  /** Checks for equality with the object’s `pArchiv` field. */
  pArchiv?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pHodnoceni` field. */
  pHodnoceni?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pId` field. */
  pId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pIdPartner` field. */
  pIdPartner?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pIdPartnerka` field. */
  pIdPartnerka?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pLatBody` field. */
  pLatBody?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pLatFinale` field. */
  pLatFinale?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pLatTrida` field. */
  pLatTrida?: InputMaybe<ParyPLatTrida>;
  /** Checks for equality with the object’s `pSttBody` field. */
  pSttBody?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pSttFinale` field. */
  pSttFinale?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pSttTrida` field. */
  pSttTrida?: InputMaybe<ParyPSttTrida>;
  /** Checks for equality with the object’s `pTimestampAdd` field. */
  pTimestampAdd?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `pTimestampArchive` field. */
  pTimestampArchive?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `Pary` */
export type ParyInput = {
  pArchiv?: InputMaybe<Scalars['Boolean']>;
  pHodnoceni?: InputMaybe<Scalars['Int']>;
  pId?: InputMaybe<Scalars['BigInt']>;
  pIdPartner: Scalars['BigInt'];
  pIdPartnerka?: InputMaybe<Scalars['BigInt']>;
  pLatBody?: InputMaybe<Scalars['Int']>;
  pLatFinale?: InputMaybe<Scalars['Boolean']>;
  pLatTrida?: InputMaybe<ParyPLatTrida>;
  pSttBody?: InputMaybe<Scalars['Int']>;
  pSttFinale?: InputMaybe<Scalars['Boolean']>;
  pSttTrida?: InputMaybe<ParyPSttTrida>;
  pTimestampAdd?: InputMaybe<Scalars['Datetime']>;
  pTimestampArchive?: InputMaybe<Scalars['Datetime']>;
};

export type ParyNavrh = Node & {
  __typename?: 'ParyNavrh';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pnId: Scalars['BigInt'];
  pnNavrhl: Scalars['BigInt'];
  pnPartner: Scalars['BigInt'];
  pnPartnerka: Scalars['BigInt'];
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnNavrhl?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartner?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartnerka?: Maybe<User>;
};

/**
 * A condition to be used against `ParyNavrh` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type ParyNavrhCondition = {
  /** Checks for equality with the object’s `pnId` field. */
  pnId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pnNavrhl` field. */
  pnNavrhl?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pnPartner` field. */
  pnPartner?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pnPartnerka` field. */
  pnPartnerka?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `ParyNavrh` */
export type ParyNavrhInput = {
  pnId?: InputMaybe<Scalars['BigInt']>;
  pnNavrhl: Scalars['BigInt'];
  pnPartner: Scalars['BigInt'];
  pnPartnerka: Scalars['BigInt'];
};

/** Represents an update to a `ParyNavrh`. Fields that are set will be updated. */
export type ParyNavrhPatch = {
  pnId?: InputMaybe<Scalars['BigInt']>;
  pnNavrhl?: InputMaybe<Scalars['BigInt']>;
  pnPartner?: InputMaybe<Scalars['BigInt']>;
  pnPartnerka?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `ParyNavrh` values. */
export type ParyNavrhsConnection = {
  __typename?: 'ParyNavrhsConnection';
  /** A list of edges which contains the `ParyNavrh` and cursor to aid in pagination. */
  edges: Array<ParyNavrhsEdge>;
  /** A list of `ParyNavrh` objects. */
  nodes: Array<ParyNavrh>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `ParyNavrh` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `ParyNavrh` edge in the connection. */
export type ParyNavrhsEdge = {
  __typename?: 'ParyNavrhsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `ParyNavrh` at the end of the edge. */
  node: ParyNavrh;
};

/** Methods to use when ordering `ParyNavrh`. */
export enum ParyNavrhsOrderBy {
  Natural = 'NATURAL',
  PnIdAsc = 'PN_ID_ASC',
  PnIdDesc = 'PN_ID_DESC',
  PnNavrhlAsc = 'PN_NAVRHL_ASC',
  PnNavrhlDesc = 'PN_NAVRHL_DESC',
  PnPartnerkaAsc = 'PN_PARTNERKA_ASC',
  PnPartnerkaDesc = 'PN_PARTNERKA_DESC',
  PnPartnerAsc = 'PN_PARTNER_ASC',
  PnPartnerDesc = 'PN_PARTNER_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export enum ParyPLatTrida {
  A = 'A',
  B = 'B',
  C = 'C',
  D = 'D',
  H = 'H',
  M = 'M',
  Z = 'Z'
}

export enum ParyPSttTrida {
  A = 'A',
  B = 'B',
  C = 'C',
  D = 'D',
  H = 'H',
  M = 'M',
  Z = 'Z'
}

/** Represents an update to a `Pary`. Fields that are set will be updated. */
export type ParyPatch = {
  pArchiv?: InputMaybe<Scalars['Boolean']>;
  pHodnoceni?: InputMaybe<Scalars['Int']>;
  pId?: InputMaybe<Scalars['BigInt']>;
  pIdPartner?: InputMaybe<Scalars['BigInt']>;
  pIdPartnerka?: InputMaybe<Scalars['BigInt']>;
  pLatBody?: InputMaybe<Scalars['Int']>;
  pLatFinale?: InputMaybe<Scalars['Boolean']>;
  pLatTrida?: InputMaybe<ParyPLatTrida>;
  pSttBody?: InputMaybe<Scalars['Int']>;
  pSttFinale?: InputMaybe<Scalars['Boolean']>;
  pSttTrida?: InputMaybe<ParyPSttTrida>;
  pTimestampAdd?: InputMaybe<Scalars['Datetime']>;
  pTimestampArchive?: InputMaybe<Scalars['Datetime']>;
};

export type Permission = Node & {
  __typename?: 'Permission';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  peAkce: Scalars['Int'];
  peAktuality: Scalars['Int'];
  peAnkety: Scalars['Int'];
  peDescription: Scalars['String'];
  peDokumenty: Scalars['Int'];
  peGalerie: Scalars['Int'];
  peId: Scalars['BigInt'];
  peInzerce: Scalars['Int'];
  peKonzole: Scalars['Int'];
  peMain: Scalars['Int'];
  peNabidka: Scalars['Int'];
  peName: Scalars['String'];
  peNastenka: Scalars['Int'];
  peNovinky: Scalars['Int'];
  pePary: Scalars['Int'];
  pePermissions: Scalars['Int'];
  pePlatby: Scalars['Int'];
  peRozpis: Scalars['Int'];
  peSkupiny: Scalars['Int'];
  peUsers: Scalars['Int'];
  /** Reads and enables pagination through a set of `User`. */
  usersByUGroup: UsersConnection;
};


export type PermissionUsersByUGroupArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UserCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};

/**
 * A condition to be used against `Permission` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type PermissionCondition = {
  /** Checks for equality with the object’s `peAkce` field. */
  peAkce?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peAktuality` field. */
  peAktuality?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peAnkety` field. */
  peAnkety?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peDescription` field. */
  peDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `peDokumenty` field. */
  peDokumenty?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peGalerie` field. */
  peGalerie?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peId` field. */
  peId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `peInzerce` field. */
  peInzerce?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peKonzole` field. */
  peKonzole?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peMain` field. */
  peMain?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peNabidka` field. */
  peNabidka?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peName` field. */
  peName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `peNastenka` field. */
  peNastenka?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peNovinky` field. */
  peNovinky?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pePary` field. */
  pePary?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pePermissions` field. */
  pePermissions?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `pePlatby` field. */
  pePlatby?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peRozpis` field. */
  peRozpis?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peSkupiny` field. */
  peSkupiny?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `peUsers` field. */
  peUsers?: InputMaybe<Scalars['Int']>;
};

/** An input for mutations affecting `Permission` */
export type PermissionInput = {
  peAkce: Scalars['Int'];
  peAktuality: Scalars['Int'];
  peAnkety: Scalars['Int'];
  peDescription: Scalars['String'];
  peDokumenty: Scalars['Int'];
  peGalerie: Scalars['Int'];
  peId?: InputMaybe<Scalars['BigInt']>;
  peInzerce: Scalars['Int'];
  peKonzole: Scalars['Int'];
  peMain: Scalars['Int'];
  peNabidka: Scalars['Int'];
  peName: Scalars['String'];
  peNastenka: Scalars['Int'];
  peNovinky: Scalars['Int'];
  pePary: Scalars['Int'];
  pePermissions: Scalars['Int'];
  pePlatby: Scalars['Int'];
  peRozpis: Scalars['Int'];
  peSkupiny: Scalars['Int'];
  peUsers: Scalars['Int'];
};

/** Represents an update to a `Permission`. Fields that are set will be updated. */
export type PermissionPatch = {
  peAkce?: InputMaybe<Scalars['Int']>;
  peAktuality?: InputMaybe<Scalars['Int']>;
  peAnkety?: InputMaybe<Scalars['Int']>;
  peDescription?: InputMaybe<Scalars['String']>;
  peDokumenty?: InputMaybe<Scalars['Int']>;
  peGalerie?: InputMaybe<Scalars['Int']>;
  peId?: InputMaybe<Scalars['BigInt']>;
  peInzerce?: InputMaybe<Scalars['Int']>;
  peKonzole?: InputMaybe<Scalars['Int']>;
  peMain?: InputMaybe<Scalars['Int']>;
  peNabidka?: InputMaybe<Scalars['Int']>;
  peName?: InputMaybe<Scalars['String']>;
  peNastenka?: InputMaybe<Scalars['Int']>;
  peNovinky?: InputMaybe<Scalars['Int']>;
  pePary?: InputMaybe<Scalars['Int']>;
  pePermissions?: InputMaybe<Scalars['Int']>;
  pePlatby?: InputMaybe<Scalars['Int']>;
  peRozpis?: InputMaybe<Scalars['Int']>;
  peSkupiny?: InputMaybe<Scalars['Int']>;
  peUsers?: InputMaybe<Scalars['Int']>;
};

/** A connection to a list of `Permission` values. */
export type PermissionsConnection = {
  __typename?: 'PermissionsConnection';
  /** A list of edges which contains the `Permission` and cursor to aid in pagination. */
  edges: Array<PermissionsEdge>;
  /** A list of `Permission` objects. */
  nodes: Array<Permission>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Permission` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Permission` edge in the connection. */
export type PermissionsEdge = {
  __typename?: 'PermissionsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Permission` at the end of the edge. */
  node: Permission;
};

/** Methods to use when ordering `Permission`. */
export enum PermissionsOrderBy {
  Natural = 'NATURAL',
  PeAkceAsc = 'PE_AKCE_ASC',
  PeAkceDesc = 'PE_AKCE_DESC',
  PeAktualityAsc = 'PE_AKTUALITY_ASC',
  PeAktualityDesc = 'PE_AKTUALITY_DESC',
  PeAnketyAsc = 'PE_ANKETY_ASC',
  PeAnketyDesc = 'PE_ANKETY_DESC',
  PeDescriptionAsc = 'PE_DESCRIPTION_ASC',
  PeDescriptionDesc = 'PE_DESCRIPTION_DESC',
  PeDokumentyAsc = 'PE_DOKUMENTY_ASC',
  PeDokumentyDesc = 'PE_DOKUMENTY_DESC',
  PeGalerieAsc = 'PE_GALERIE_ASC',
  PeGalerieDesc = 'PE_GALERIE_DESC',
  PeIdAsc = 'PE_ID_ASC',
  PeIdDesc = 'PE_ID_DESC',
  PeInzerceAsc = 'PE_INZERCE_ASC',
  PeInzerceDesc = 'PE_INZERCE_DESC',
  PeKonzoleAsc = 'PE_KONZOLE_ASC',
  PeKonzoleDesc = 'PE_KONZOLE_DESC',
  PeMainAsc = 'PE_MAIN_ASC',
  PeMainDesc = 'PE_MAIN_DESC',
  PeNabidkaAsc = 'PE_NABIDKA_ASC',
  PeNabidkaDesc = 'PE_NABIDKA_DESC',
  PeNameAsc = 'PE_NAME_ASC',
  PeNameDesc = 'PE_NAME_DESC',
  PeNastenkaAsc = 'PE_NASTENKA_ASC',
  PeNastenkaDesc = 'PE_NASTENKA_DESC',
  PeNovinkyAsc = 'PE_NOVINKY_ASC',
  PeNovinkyDesc = 'PE_NOVINKY_DESC',
  PeParyAsc = 'PE_PARY_ASC',
  PeParyDesc = 'PE_PARY_DESC',
  PePermissionsAsc = 'PE_PERMISSIONS_ASC',
  PePermissionsDesc = 'PE_PERMISSIONS_DESC',
  PePlatbyAsc = 'PE_PLATBY_ASC',
  PePlatbyDesc = 'PE_PLATBY_DESC',
  PeRozpisAsc = 'PE_ROZPIS_ASC',
  PeRozpisDesc = 'PE_ROZPIS_DESC',
  PeSkupinyAsc = 'PE_SKUPINY_ASC',
  PeSkupinyDesc = 'PE_SKUPINY_DESC',
  PeUsersAsc = 'PE_USERS_ASC',
  PeUsersDesc = 'PE_USERS_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** A connection to a list of `PlatbyCategory` values. */
export type PlatbyCategoriesConnection = {
  __typename?: 'PlatbyCategoriesConnection';
  /** A list of edges which contains the `PlatbyCategory` and cursor to aid in pagination. */
  edges: Array<PlatbyCategoriesEdge>;
  /** A list of `PlatbyCategory` objects. */
  nodes: Array<PlatbyCategory>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyCategory` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyCategory` edge in the connection. */
export type PlatbyCategoriesEdge = {
  __typename?: 'PlatbyCategoriesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyCategory` at the end of the edge. */
  node: PlatbyCategory;
};

/** Methods to use when ordering `PlatbyCategory`. */
export enum PlatbyCategoriesOrderBy {
  Natural = 'NATURAL',
  PcAmountAsc = 'PC_AMOUNT_ASC',
  PcAmountDesc = 'PC_AMOUNT_DESC',
  PcArchiveAsc = 'PC_ARCHIVE_ASC',
  PcArchiveDesc = 'PC_ARCHIVE_DESC',
  PcDateDueAsc = 'PC_DATE_DUE_ASC',
  PcDateDueDesc = 'PC_DATE_DUE_DESC',
  PcIdAsc = 'PC_ID_ASC',
  PcIdDesc = 'PC_ID_DESC',
  PcNameAsc = 'PC_NAME_ASC',
  PcNameDesc = 'PC_NAME_DESC',
  PcSymbolAsc = 'PC_SYMBOL_ASC',
  PcSymbolDesc = 'PC_SYMBOL_DESC',
  PcUseBaseAsc = 'PC_USE_BASE_ASC',
  PcUseBaseDesc = 'PC_USE_BASE_DESC',
  PcUsePrefixAsc = 'PC_USE_PREFIX_ASC',
  PcUsePrefixDesc = 'PC_USE_PREFIX_DESC',
  PcValidFromAsc = 'PC_VALID_FROM_ASC',
  PcValidFromDesc = 'PC_VALID_FROM_DESC',
  PcValidToAsc = 'PC_VALID_TO_ASC',
  PcValidToDesc = 'PC_VALID_TO_DESC',
  PcVisibleAsc = 'PC_VISIBLE_ASC',
  PcVisibleDesc = 'PC_VISIBLE_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type PlatbyCategory = Node & {
  __typename?: 'PlatbyCategory';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pcAmount: Scalars['BigFloat'];
  pcArchive: Scalars['Boolean'];
  pcDateDue: Scalars['Date'];
  pcId: Scalars['BigInt'];
  pcName: Scalars['String'];
  pcSymbol: Scalars['BigInt'];
  pcUseBase: Scalars['Boolean'];
  pcUsePrefix: Scalars['Boolean'];
  pcValidFrom: Scalars['Date'];
  pcValidTo: Scalars['Date'];
  pcVisible: Scalars['Boolean'];
  /** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
  platbyCategoryGroupsByPcgIdCategory: PlatbyCategoryGroupsConnection;
  /** Reads and enables pagination through a set of `PlatbyItem`. */
  platbyItemsByPiIdCategory: PlatbyItemsConnection;
};


export type PlatbyCategoryPlatbyCategoryGroupsByPcgIdCategoryArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyCategoryGroupCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};


export type PlatbyCategoryPlatbyItemsByPiIdCategoryArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};

/**
 * A condition to be used against `PlatbyCategory` object types. All fields are
 * tested for equality and combined with a logical ‘and.’
 */
export type PlatbyCategoryCondition = {
  /** Checks for equality with the object’s `pcAmount` field. */
  pcAmount?: InputMaybe<Scalars['BigFloat']>;
  /** Checks for equality with the object’s `pcArchive` field. */
  pcArchive?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pcDateDue` field. */
  pcDateDue?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `pcId` field. */
  pcId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pcName` field. */
  pcName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `pcSymbol` field. */
  pcSymbol?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pcUseBase` field. */
  pcUseBase?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pcUsePrefix` field. */
  pcUsePrefix?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `pcValidFrom` field. */
  pcValidFrom?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `pcValidTo` field. */
  pcValidTo?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `pcVisible` field. */
  pcVisible?: InputMaybe<Scalars['Boolean']>;
};

export type PlatbyCategoryGroup = Node & {
  __typename?: 'PlatbyCategoryGroup';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pcgId: Scalars['BigInt'];
  pcgIdCategory: Scalars['BigInt'];
  pcgIdGroup: Scalars['BigInt'];
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
  platbyCategoryByPcgIdCategory?: Maybe<PlatbyCategory>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
  platbyGroupByPcgIdGroup?: Maybe<PlatbyGroup>;
};

/**
 * A condition to be used against `PlatbyCategoryGroup` object types. All fields
 * are tested for equality and combined with a logical ‘and.’
 */
export type PlatbyCategoryGroupCondition = {
  /** Checks for equality with the object’s `pcgId` field. */
  pcgId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pcgIdCategory` field. */
  pcgIdCategory?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pcgIdGroup` field. */
  pcgIdGroup?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `PlatbyCategoryGroup` */
export type PlatbyCategoryGroupInput = {
  pcgId?: InputMaybe<Scalars['BigInt']>;
  pcgIdCategory: Scalars['BigInt'];
  pcgIdGroup: Scalars['BigInt'];
};

/** Represents an update to a `PlatbyCategoryGroup`. Fields that are set will be updated. */
export type PlatbyCategoryGroupPatch = {
  pcgId?: InputMaybe<Scalars['BigInt']>;
  pcgIdCategory?: InputMaybe<Scalars['BigInt']>;
  pcgIdGroup?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `PlatbyCategoryGroup` values. */
export type PlatbyCategoryGroupsConnection = {
  __typename?: 'PlatbyCategoryGroupsConnection';
  /** A list of edges which contains the `PlatbyCategoryGroup` and cursor to aid in pagination. */
  edges: Array<PlatbyCategoryGroupsEdge>;
  /** A list of `PlatbyCategoryGroup` objects. */
  nodes: Array<PlatbyCategoryGroup>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyCategoryGroup` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyCategoryGroup` edge in the connection. */
export type PlatbyCategoryGroupsEdge = {
  __typename?: 'PlatbyCategoryGroupsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyCategoryGroup` at the end of the edge. */
  node: PlatbyCategoryGroup;
};

/** Methods to use when ordering `PlatbyCategoryGroup`. */
export enum PlatbyCategoryGroupsOrderBy {
  Natural = 'NATURAL',
  PcgIdAsc = 'PCG_ID_ASC',
  PcgIdCategoryAsc = 'PCG_ID_CATEGORY_ASC',
  PcgIdCategoryDesc = 'PCG_ID_CATEGORY_DESC',
  PcgIdDesc = 'PCG_ID_DESC',
  PcgIdGroupAsc = 'PCG_ID_GROUP_ASC',
  PcgIdGroupDesc = 'PCG_ID_GROUP_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** An input for mutations affecting `PlatbyCategory` */
export type PlatbyCategoryInput = {
  pcAmount: Scalars['BigFloat'];
  pcArchive?: InputMaybe<Scalars['Boolean']>;
  pcDateDue: Scalars['Date'];
  pcId?: InputMaybe<Scalars['BigInt']>;
  pcName: Scalars['String'];
  pcSymbol: Scalars['BigInt'];
  pcUseBase?: InputMaybe<Scalars['Boolean']>;
  pcUsePrefix?: InputMaybe<Scalars['Boolean']>;
  pcValidFrom: Scalars['Date'];
  pcValidTo: Scalars['Date'];
  pcVisible?: InputMaybe<Scalars['Boolean']>;
};

/** Represents an update to a `PlatbyCategory`. Fields that are set will be updated. */
export type PlatbyCategoryPatch = {
  pcAmount?: InputMaybe<Scalars['BigFloat']>;
  pcArchive?: InputMaybe<Scalars['Boolean']>;
  pcDateDue?: InputMaybe<Scalars['Date']>;
  pcId?: InputMaybe<Scalars['BigInt']>;
  pcName?: InputMaybe<Scalars['String']>;
  pcSymbol?: InputMaybe<Scalars['BigInt']>;
  pcUseBase?: InputMaybe<Scalars['Boolean']>;
  pcUsePrefix?: InputMaybe<Scalars['Boolean']>;
  pcValidFrom?: InputMaybe<Scalars['Date']>;
  pcValidTo?: InputMaybe<Scalars['Date']>;
  pcVisible?: InputMaybe<Scalars['Boolean']>;
};

export type PlatbyGroup = Node & {
  __typename?: 'PlatbyGroup';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pgBase: Scalars['BigInt'];
  pgDescription: Scalars['String'];
  pgId: Scalars['BigInt'];
  pgName: Scalars['String'];
  pgType: Scalars['BigFloat'];
  /** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
  platbyCategoryGroupsByPcgIdGroup: PlatbyCategoryGroupsConnection;
  /** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
  platbyGroupSkupinasByPgsIdGroup: PlatbyGroupSkupinasConnection;
};


export type PlatbyGroupPlatbyCategoryGroupsByPcgIdGroupArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyCategoryGroupCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};


export type PlatbyGroupPlatbyGroupSkupinasByPgsIdGroupArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyGroupSkupinaCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};

/**
 * A condition to be used against `PlatbyGroup` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type PlatbyGroupCondition = {
  /** Checks for equality with the object’s `pgBase` field. */
  pgBase?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pgDescription` field. */
  pgDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `pgId` field. */
  pgId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pgName` field. */
  pgName?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `pgType` field. */
  pgType?: InputMaybe<Scalars['BigFloat']>;
};

/** An input for mutations affecting `PlatbyGroup` */
export type PlatbyGroupInput = {
  pgBase?: InputMaybe<Scalars['BigInt']>;
  pgDescription: Scalars['String'];
  pgId?: InputMaybe<Scalars['BigInt']>;
  pgName: Scalars['String'];
  pgType?: InputMaybe<Scalars['BigFloat']>;
};

/** Represents an update to a `PlatbyGroup`. Fields that are set will be updated. */
export type PlatbyGroupPatch = {
  pgBase?: InputMaybe<Scalars['BigInt']>;
  pgDescription?: InputMaybe<Scalars['String']>;
  pgId?: InputMaybe<Scalars['BigInt']>;
  pgName?: InputMaybe<Scalars['String']>;
  pgType?: InputMaybe<Scalars['BigFloat']>;
};

export type PlatbyGroupSkupina = Node & {
  __typename?: 'PlatbyGroupSkupina';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  pgsId: Scalars['BigInt'];
  pgsIdGroup: Scalars['BigInt'];
  pgsIdSkupina: Scalars['BigInt'];
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
  platbyGroupByPgsIdGroup?: Maybe<PlatbyGroup>;
  /** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
  skupinyByPgsIdSkupina?: Maybe<Skupiny>;
};

/**
 * A condition to be used against `PlatbyGroupSkupina` object types. All fields are
 * tested for equality and combined with a logical ‘and.’
 */
export type PlatbyGroupSkupinaCondition = {
  /** Checks for equality with the object’s `pgsId` field. */
  pgsId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pgsIdGroup` field. */
  pgsIdGroup?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `pgsIdSkupina` field. */
  pgsIdSkupina?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `PlatbyGroupSkupina` */
export type PlatbyGroupSkupinaInput = {
  pgsId?: InputMaybe<Scalars['BigInt']>;
  pgsIdGroup: Scalars['BigInt'];
  pgsIdSkupina: Scalars['BigInt'];
};

/** Represents an update to a `PlatbyGroupSkupina`. Fields that are set will be updated. */
export type PlatbyGroupSkupinaPatch = {
  pgsId?: InputMaybe<Scalars['BigInt']>;
  pgsIdGroup?: InputMaybe<Scalars['BigInt']>;
  pgsIdSkupina?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `PlatbyGroupSkupina` values. */
export type PlatbyGroupSkupinasConnection = {
  __typename?: 'PlatbyGroupSkupinasConnection';
  /** A list of edges which contains the `PlatbyGroupSkupina` and cursor to aid in pagination. */
  edges: Array<PlatbyGroupSkupinasEdge>;
  /** A list of `PlatbyGroupSkupina` objects. */
  nodes: Array<PlatbyGroupSkupina>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyGroupSkupina` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyGroupSkupina` edge in the connection. */
export type PlatbyGroupSkupinasEdge = {
  __typename?: 'PlatbyGroupSkupinasEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyGroupSkupina` at the end of the edge. */
  node: PlatbyGroupSkupina;
};

/** Methods to use when ordering `PlatbyGroupSkupina`. */
export enum PlatbyGroupSkupinasOrderBy {
  Natural = 'NATURAL',
  PgsIdAsc = 'PGS_ID_ASC',
  PgsIdDesc = 'PGS_ID_DESC',
  PgsIdGroupAsc = 'PGS_ID_GROUP_ASC',
  PgsIdGroupDesc = 'PGS_ID_GROUP_DESC',
  PgsIdSkupinaAsc = 'PGS_ID_SKUPINA_ASC',
  PgsIdSkupinaDesc = 'PGS_ID_SKUPINA_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

/** A connection to a list of `PlatbyGroup` values. */
export type PlatbyGroupsConnection = {
  __typename?: 'PlatbyGroupsConnection';
  /** A list of edges which contains the `PlatbyGroup` and cursor to aid in pagination. */
  edges: Array<PlatbyGroupsEdge>;
  /** A list of `PlatbyGroup` objects. */
  nodes: Array<PlatbyGroup>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyGroup` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyGroup` edge in the connection. */
export type PlatbyGroupsEdge = {
  __typename?: 'PlatbyGroupsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyGroup` at the end of the edge. */
  node: PlatbyGroup;
};

/** Methods to use when ordering `PlatbyGroup`. */
export enum PlatbyGroupsOrderBy {
  Natural = 'NATURAL',
  PgBaseAsc = 'PG_BASE_ASC',
  PgBaseDesc = 'PG_BASE_DESC',
  PgDescriptionAsc = 'PG_DESCRIPTION_ASC',
  PgDescriptionDesc = 'PG_DESCRIPTION_DESC',
  PgIdAsc = 'PG_ID_ASC',
  PgIdDesc = 'PG_ID_DESC',
  PgNameAsc = 'PG_NAME_ASC',
  PgNameDesc = 'PG_NAME_DESC',
  PgTypeAsc = 'PG_TYPE_ASC',
  PgTypeDesc = 'PG_TYPE_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type PlatbyItem = Node & {
  __typename?: 'PlatbyItem';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  piAmount: Scalars['BigFloat'];
  piDate: Scalars['Date'];
  piId: Scalars['BigInt'];
  piIdCategory: Scalars['BigInt'];
  piIdRaw?: Maybe<Scalars['BigInt']>;
  piIdUser?: Maybe<Scalars['BigInt']>;
  piPrefix: Scalars['Int'];
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
  platbyCategoryByPiIdCategory?: Maybe<PlatbyCategory>;
  /** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
  platbyRawByPiIdRaw?: Maybe<PlatbyRaw>;
  /** Reads a single `User` that is related to this `PlatbyItem`. */
  userByPiIdUser?: Maybe<User>;
};

/**
 * A condition to be used against `PlatbyItem` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type PlatbyItemCondition = {
  /** Checks for equality with the object’s `piAmount` field. */
  piAmount?: InputMaybe<Scalars['BigFloat']>;
  /** Checks for equality with the object’s `piDate` field. */
  piDate?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `piId` field. */
  piId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `piIdCategory` field. */
  piIdCategory?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `piIdRaw` field. */
  piIdRaw?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `piIdUser` field. */
  piIdUser?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `piPrefix` field. */
  piPrefix?: InputMaybe<Scalars['Int']>;
};

/** An input for mutations affecting `PlatbyItem` */
export type PlatbyItemInput = {
  piAmount: Scalars['BigFloat'];
  piDate: Scalars['Date'];
  piId?: InputMaybe<Scalars['BigInt']>;
  piIdCategory: Scalars['BigInt'];
  piIdRaw?: InputMaybe<Scalars['BigInt']>;
  piIdUser?: InputMaybe<Scalars['BigInt']>;
  piPrefix?: InputMaybe<Scalars['Int']>;
};

/** Represents an update to a `PlatbyItem`. Fields that are set will be updated. */
export type PlatbyItemPatch = {
  piAmount?: InputMaybe<Scalars['BigFloat']>;
  piDate?: InputMaybe<Scalars['Date']>;
  piId?: InputMaybe<Scalars['BigInt']>;
  piIdCategory?: InputMaybe<Scalars['BigInt']>;
  piIdRaw?: InputMaybe<Scalars['BigInt']>;
  piIdUser?: InputMaybe<Scalars['BigInt']>;
  piPrefix?: InputMaybe<Scalars['Int']>;
};

/** A connection to a list of `PlatbyItem` values. */
export type PlatbyItemsConnection = {
  __typename?: 'PlatbyItemsConnection';
  /** A list of edges which contains the `PlatbyItem` and cursor to aid in pagination. */
  edges: Array<PlatbyItemsEdge>;
  /** A list of `PlatbyItem` objects. */
  nodes: Array<PlatbyItem>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyItem` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyItem` edge in the connection. */
export type PlatbyItemsEdge = {
  __typename?: 'PlatbyItemsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyItem` at the end of the edge. */
  node: PlatbyItem;
};

/** Methods to use when ordering `PlatbyItem`. */
export enum PlatbyItemsOrderBy {
  Natural = 'NATURAL',
  PiAmountAsc = 'PI_AMOUNT_ASC',
  PiAmountDesc = 'PI_AMOUNT_DESC',
  PiDateAsc = 'PI_DATE_ASC',
  PiDateDesc = 'PI_DATE_DESC',
  PiIdAsc = 'PI_ID_ASC',
  PiIdCategoryAsc = 'PI_ID_CATEGORY_ASC',
  PiIdCategoryDesc = 'PI_ID_CATEGORY_DESC',
  PiIdDesc = 'PI_ID_DESC',
  PiIdRawAsc = 'PI_ID_RAW_ASC',
  PiIdRawDesc = 'PI_ID_RAW_DESC',
  PiIdUserAsc = 'PI_ID_USER_ASC',
  PiIdUserDesc = 'PI_ID_USER_DESC',
  PiPrefixAsc = 'PI_PREFIX_ASC',
  PiPrefixDesc = 'PI_PREFIX_DESC',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC'
}

export type PlatbyRaw = Node & {
  __typename?: 'PlatbyRaw';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads and enables pagination through a set of `PlatbyItem`. */
  platbyItemsByPiIdRaw: PlatbyItemsConnection;
  prDiscarded: Scalars['Boolean'];
  prHash: Scalars['String'];
  prId: Scalars['BigInt'];
  prRaw: Scalars['String'];
  prSorted: Scalars['Boolean'];
};


export type PlatbyRawPlatbyItemsByPiIdRawArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};

/**
 * A condition to be used against `PlatbyRaw` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type PlatbyRawCondition = {
  /** Checks for equality with the object’s `prDiscarded` field. */
  prDiscarded?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `prHash` field. */
  prHash?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `prId` field. */
  prId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `prRaw` field. */
  prRaw?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `prSorted` field. */
  prSorted?: InputMaybe<Scalars['Boolean']>;
};

/** An input for mutations affecting `PlatbyRaw` */
export type PlatbyRawInput = {
  prDiscarded?: InputMaybe<Scalars['Boolean']>;
  prHash: Scalars['String'];
  prId?: InputMaybe<Scalars['BigInt']>;
  prRaw: Scalars['String'];
  prSorted?: InputMaybe<Scalars['Boolean']>;
};

/** Represents an update to a `PlatbyRaw`. Fields that are set will be updated. */
export type PlatbyRawPatch = {
  prDiscarded?: InputMaybe<Scalars['Boolean']>;
  prHash?: InputMaybe<Scalars['String']>;
  prId?: InputMaybe<Scalars['BigInt']>;
  prRaw?: InputMaybe<Scalars['String']>;
  prSorted?: InputMaybe<Scalars['Boolean']>;
};

/** A connection to a list of `PlatbyRaw` values. */
export type PlatbyRawsConnection = {
  __typename?: 'PlatbyRawsConnection';
  /** A list of edges which contains the `PlatbyRaw` and cursor to aid in pagination. */
  edges: Array<PlatbyRawsEdge>;
  /** A list of `PlatbyRaw` objects. */
  nodes: Array<PlatbyRaw>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `PlatbyRaw` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `PlatbyRaw` edge in the connection. */
export type PlatbyRawsEdge = {
  __typename?: 'PlatbyRawsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `PlatbyRaw` at the end of the edge. */
  node: PlatbyRaw;
};

/** Methods to use when ordering `PlatbyRaw`. */
export enum PlatbyRawsOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  PrDiscardedAsc = 'PR_DISCARDED_ASC',
  PrDiscardedDesc = 'PR_DISCARDED_DESC',
  PrHashAsc = 'PR_HASH_ASC',
  PrHashDesc = 'PR_HASH_DESC',
  PrIdAsc = 'PR_ID_ASC',
  PrIdDesc = 'PR_ID_DESC',
  PrRawAsc = 'PR_RAW_ASC',
  PrRawDesc = 'PR_RAW_DESC',
  PrSortedAsc = 'PR_SORTED_ASC',
  PrSortedDesc = 'PR_SORTED_DESC'
}

/** The root query type which gives access points into the data universe. */
export type Query = Node & {
  __typename?: 'Query';
  /** Reads a single `Akce` using its globally unique `ID`. */
  akce?: Maybe<Akce>;
  akceByAId?: Maybe<Akce>;
  /** Reads a single `AkceItem` using its globally unique `ID`. */
  akceItem?: Maybe<AkceItem>;
  akceItemByAiId?: Maybe<AkceItem>;
  /** Reads a single `Aktuality` using its globally unique `ID`. */
  aktuality?: Maybe<Aktuality>;
  aktualityByAtId?: Maybe<Aktuality>;
  /** Reads and enables pagination through a set of `AkceItem`. */
  allAkceItems?: Maybe<AkceItemsConnection>;
  /** Reads and enables pagination through a set of `Akce`. */
  allAkces?: Maybe<AkcesConnection>;
  /** Reads and enables pagination through a set of `Aktuality`. */
  allAktualities?: Maybe<AktualitiesConnection>;
  /** Reads and enables pagination through a set of `Dokumenty`. */
  allDokumenties?: Maybe<DokumentiesConnection>;
  /** Reads and enables pagination through a set of `GalerieDir`. */
  allGalerieDirs?: Maybe<GalerieDirsConnection>;
  /** Reads and enables pagination through a set of `GalerieFoto`. */
  allGalerieFotos?: Maybe<GalerieFotosConnection>;
  /** Reads and enables pagination through a set of `Member`. */
  allMembers?: Maybe<MembersConnection>;
  /** Reads and enables pagination through a set of `NabidkaItem`. */
  allNabidkaItems?: Maybe<NabidkaItemsConnection>;
  /** Reads and enables pagination through a set of `Nabidka`. */
  allNabidkas?: Maybe<NabidkasConnection>;
  /** Reads and enables pagination through a set of `PageRevision`. */
  allPageRevisions?: Maybe<PageRevisionsConnection>;
  /** Reads and enables pagination through a set of `Page`. */
  allPages?: Maybe<PagesConnection>;
  /** Reads and enables pagination through a set of `Parameter`. */
  allParameters?: Maybe<ParametersConnection>;
  /** Reads and enables pagination through a set of `Pary`. */
  allParies?: Maybe<PariesConnection>;
  /** Reads and enables pagination through a set of `ParyNavrh`. */
  allParyNavrhs?: Maybe<ParyNavrhsConnection>;
  /** Reads and enables pagination through a set of `Permission`. */
  allPermissions?: Maybe<PermissionsConnection>;
  /** Reads and enables pagination through a set of `PlatbyCategory`. */
  allPlatbyCategories?: Maybe<PlatbyCategoriesConnection>;
  /** Reads and enables pagination through a set of `PlatbyCategoryGroup`. */
  allPlatbyCategoryGroups?: Maybe<PlatbyCategoryGroupsConnection>;
  /** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
  allPlatbyGroupSkupinas?: Maybe<PlatbyGroupSkupinasConnection>;
  /** Reads and enables pagination through a set of `PlatbyGroup`. */
  allPlatbyGroups?: Maybe<PlatbyGroupsConnection>;
  /** Reads and enables pagination through a set of `PlatbyItem`. */
  allPlatbyItems?: Maybe<PlatbyItemsConnection>;
  /** Reads and enables pagination through a set of `PlatbyRaw`. */
  allPlatbyRaws?: Maybe<PlatbyRawsConnection>;
  /** Reads and enables pagination through a set of `Rozpi`. */
  allRozpis?: Maybe<RozpisConnection>;
  /** Reads and enables pagination through a set of `RozpisItem`. */
  allRozpisItems?: Maybe<RozpisItemsConnection>;
  /** Reads and enables pagination through a set of `Session`. */
  allSessions?: Maybe<SessionsConnection>;
  /** Reads and enables pagination through a set of `Skupiny`. */
  allSkupinies?: Maybe<SkupiniesConnection>;
  /** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
  allUpozorneniSkupinies?: Maybe<UpozorneniSkupiniesConnection>;
  /** Reads and enables pagination through a set of `Upozorneni`. */
  allUpozornenis?: Maybe<UpozornenisConnection>;
  /** Reads and enables pagination through a set of `User`. */
  allUsers?: Maybe<UsersConnection>;
  /** Reads and enables pagination through a set of `UsersSkupiny`. */
  allUsersSkupinies?: Maybe<UsersSkupiniesConnection>;
  /** Reads and enables pagination through a set of `VideoList`. */
  allVideoLists?: Maybe<VideoListsConnection>;
  /** Reads and enables pagination through a set of `VideoSource`. */
  allVideoSources?: Maybe<VideoSourcesConnection>;
  /** Reads and enables pagination through a set of `Video`. */
  allVideos?: Maybe<VideosConnection>;
  currentCoupleIds?: Maybe<CurrentCoupleIdsConnection>;
  currentSessionId?: Maybe<Scalars['String']>;
  currentUserId?: Maybe<Scalars['BigInt']>;
  /** Reads a single `Dokumenty` using its globally unique `ID`. */
  dokumenty?: Maybe<Dokumenty>;
  dokumentyByDId?: Maybe<Dokumenty>;
  /** Reads a single `GalerieDir` using its globally unique `ID`. */
  galerieDir?: Maybe<GalerieDir>;
  galerieDirByGdId?: Maybe<GalerieDir>;
  /** Reads a single `GalerieFoto` using its globally unique `ID`. */
  galerieFoto?: Maybe<GalerieFoto>;
  galerieFotoByGfId?: Maybe<GalerieFoto>;
  getCurrentUser?: Maybe<User>;
  /** Reads a single `Nabidka` using its globally unique `ID`. */
  nabidka?: Maybe<Nabidka>;
  nabidkaByNId?: Maybe<Nabidka>;
  /** Reads a single `NabidkaItem` using its globally unique `ID`. */
  nabidkaItem?: Maybe<NabidkaItem>;
  nabidkaItemByNiId?: Maybe<NabidkaItem>;
  /** Fetches an object given its globally unique `ID`. */
  node?: Maybe<Node>;
  /** The root query type must be a `Node` to work well with Relay 1 mutations. This just resolves to `query`. */
  nodeId: Scalars['ID'];
  /** Reads a single `Page` using its globally unique `ID`. */
  page?: Maybe<Page>;
  pageById?: Maybe<Page>;
  pageByUrl?: Maybe<Page>;
  /** Reads a single `PageRevision` using its globally unique `ID`. */
  pageRevision?: Maybe<PageRevision>;
  pageRevisionByRevNumberAndId?: Maybe<PageRevision>;
  /** Reads a single `Parameter` using its globally unique `ID`. */
  parameter?: Maybe<Parameter>;
  parameterByPaName?: Maybe<Parameter>;
  /** Reads a single `Pary` using its globally unique `ID`. */
  pary?: Maybe<Pary>;
  paryByPId?: Maybe<Pary>;
  /** Reads a single `ParyNavrh` using its globally unique `ID`. */
  paryNavrh?: Maybe<ParyNavrh>;
  paryNavrhByPnId?: Maybe<ParyNavrh>;
  /** Reads a single `Permission` using its globally unique `ID`. */
  permission?: Maybe<Permission>;
  permissionByPeId?: Maybe<Permission>;
  /** Reads a single `PlatbyCategory` using its globally unique `ID`. */
  platbyCategory?: Maybe<PlatbyCategory>;
  platbyCategoryByPcId?: Maybe<PlatbyCategory>;
  /** Reads a single `PlatbyCategoryGroup` using its globally unique `ID`. */
  platbyCategoryGroup?: Maybe<PlatbyCategoryGroup>;
  platbyCategoryGroupByPcgId?: Maybe<PlatbyCategoryGroup>;
  /** Reads a single `PlatbyGroup` using its globally unique `ID`. */
  platbyGroup?: Maybe<PlatbyGroup>;
  platbyGroupByPgId?: Maybe<PlatbyGroup>;
  /** Reads a single `PlatbyGroupSkupina` using its globally unique `ID`. */
  platbyGroupSkupina?: Maybe<PlatbyGroupSkupina>;
  platbyGroupSkupinaByPgsId?: Maybe<PlatbyGroupSkupina>;
  /** Reads a single `PlatbyItem` using its globally unique `ID`. */
  platbyItem?: Maybe<PlatbyItem>;
  platbyItemByPiId?: Maybe<PlatbyItem>;
  /** Reads a single `PlatbyRaw` using its globally unique `ID`. */
  platbyRaw?: Maybe<PlatbyRaw>;
  platbyRawByPrId?: Maybe<PlatbyRaw>;
  /**
   * Exposes the root query type nested one level down. This is helpful for Relay 1
   * which can only query top level fields if they are in a particular form.
   */
  query: Query;
  /** Reads a single `Rozpi` using its globally unique `ID`. */
  rozpi?: Maybe<Rozpi>;
  rozpiByRId?: Maybe<Rozpi>;
  /** Reads a single `RozpisItem` using its globally unique `ID`. */
  rozpisItem?: Maybe<RozpisItem>;
  rozpisItemByRiId?: Maybe<RozpisItem>;
  /** Reads a single `Session` using its globally unique `ID`. */
  session?: Maybe<Session>;
  sessionBySsId?: Maybe<Session>;
  /** Reads a single `Skupiny` using its globally unique `ID`. */
  skupiny?: Maybe<Skupiny>;
  skupinyBySId?: Maybe<Skupiny>;
  /** Reads a single `Upozorneni` using its globally unique `ID`. */
  upozorneni?: Maybe<Upozorneni>;
  upozorneniByUpId?: Maybe<Upozorneni>;
  /** Reads a single `UpozorneniSkupiny` using its globally unique `ID`. */
  upozorneniSkupiny?: Maybe<UpozorneniSkupiny>;
  upozorneniSkupinyByUpsId?: Maybe<UpozorneniSkupiny>;
  /** Reads a single `User` using its globally unique `ID`. */
  user?: Maybe<User>;
  userByUId?: Maybe<User>;
  /** Reads a single `UsersSkupiny` using its globally unique `ID`. */
  usersSkupiny?: Maybe<UsersSkupiny>;
  usersSkupinyByUsId?: Maybe<UsersSkupiny>;
  /** Reads a single `Video` using its globally unique `ID`. */
  video?: Maybe<Video>;
  videoByVId?: Maybe<Video>;
  /** Reads a single `VideoList` using its globally unique `ID`. */
  videoList?: Maybe<VideoList>;
  videoListByVlId?: Maybe<VideoList>;
  /** Reads a single `VideoSource` using its globally unique `ID`. */
  videoSource?: Maybe<VideoSource>;
  videoSourceByVsId?: Maybe<VideoSource>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAkceArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAkceByAIdArgs = {
  aId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAkceItemArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAkceItemByAiIdArgs = {
  aiId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAktualityArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAktualityByAtIdArgs = {
  atId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryAllAkceItemsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AkceItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllAkcesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AkceCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AkcesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllAktualitiesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AktualityCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllDokumentiesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<DokumentyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<DokumentiesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllGalerieDirsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<GalerieDirCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<GalerieDirsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllGalerieFotosArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<GalerieFotoCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllMembersArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<MemberCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<MembersOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllNabidkaItemsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<NabidkaItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllNabidkasArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<NabidkaCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<NabidkasOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPageRevisionsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PageRevisionCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PageRevisionsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPagesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PageCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PagesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllParametersArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParameterCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<ParametersOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPariesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PariesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllParyNavrhsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyNavrhCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPermissionsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PermissionCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PermissionsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyCategoriesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyCategoryCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyCategoriesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyCategoryGroupsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyCategoryGroupCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyGroupSkupinasArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyGroupSkupinaCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyGroupsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyGroupCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyGroupsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyItemsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllPlatbyRawsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyRawCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyRawsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllRozpisArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<RozpiCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<RozpisOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllRozpisItemsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<RozpisItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllSessionsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<SessionCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<SessionsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllSkupiniesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<SkupinyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<SkupiniesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllUpozorneniSkupiniesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UpozorneniSkupinyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllUpozornenisArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UpozorneniCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UpozornenisOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllUsersArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UserCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllUsersSkupiniesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UsersSkupinyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UsersSkupiniesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllVideoListsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<VideoListCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<VideoListsOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllVideoSourcesArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<VideoSourceCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<VideoSourcesOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryAllVideosArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<VideoCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<VideosOrderBy>>;
};


/** The root query type which gives access points into the data universe. */
export type QueryCurrentCoupleIdsArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
};


/** The root query type which gives access points into the data universe. */
export type QueryDokumentyArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryDokumentyByDIdArgs = {
  dId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryGalerieDirArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryGalerieDirByGdIdArgs = {
  gdId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryGalerieFotoArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryGalerieFotoByGfIdArgs = {
  gfId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryNabidkaArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryNabidkaByNIdArgs = {
  nId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryNabidkaItemArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryNabidkaItemByNiIdArgs = {
  niId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryNodeArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPageArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPageByIdArgs = {
  id: Scalars['Int'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPageByUrlArgs = {
  url: Scalars['String'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPageRevisionArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPageRevisionByRevNumberAndIdArgs = {
  id: Scalars['Int'];
  revNumber: Scalars['Int'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParameterArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParameterByPaNameArgs = {
  paName: Scalars['String'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParyArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParyByPIdArgs = {
  pId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParyNavrhArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryParyNavrhByPnIdArgs = {
  pnId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPermissionArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPermissionByPeIdArgs = {
  peId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyCategoryArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyCategoryByPcIdArgs = {
  pcId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyCategoryGroupArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyCategoryGroupByPcgIdArgs = {
  pcgId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyGroupArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyGroupByPgIdArgs = {
  pgId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyGroupSkupinaArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyGroupSkupinaByPgsIdArgs = {
  pgsId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyItemArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyItemByPiIdArgs = {
  piId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyRawArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryPlatbyRawByPrIdArgs = {
  prId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryRozpiArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryRozpiByRIdArgs = {
  rId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryRozpisItemArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryRozpisItemByRiIdArgs = {
  riId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QuerySessionArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QuerySessionBySsIdArgs = {
  ssId: Scalars['String'];
};


/** The root query type which gives access points into the data universe. */
export type QuerySkupinyArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QuerySkupinyBySIdArgs = {
  sId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUpozorneniArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUpozorneniByUpIdArgs = {
  upId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUpozorneniSkupinyArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUpozorneniSkupinyByUpsIdArgs = {
  upsId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUserArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUserByUIdArgs = {
  uId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUsersSkupinyArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryUsersSkupinyByUsIdArgs = {
  usId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoByVIdArgs = {
  vId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoListArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoListByVlIdArgs = {
  vlId: Scalars['BigInt'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoSourceArgs = {
  nodeId: Scalars['ID'];
};


/** The root query type which gives access points into the data universe. */
export type QueryVideoSourceByVsIdArgs = {
  vsId: Scalars['BigInt'];
};

export type Rozpi = Node & {
  __typename?: 'Rozpi';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  rDatum: Scalars['Date'];
  rId: Scalars['BigInt'];
  rKde: Scalars['String'];
  rLock: Scalars['Boolean'];
  rTimestamp?: Maybe<Scalars['Datetime']>;
  rTrener: Scalars['BigInt'];
  rVisible: Scalars['Boolean'];
  /** Reads and enables pagination through a set of `RozpisItem`. */
  rozpisItemsByRiIdRodic: RozpisItemsConnection;
  /** Reads a single `User` that is related to this `Rozpi`. */
  userByRTrener?: Maybe<User>;
};


export type RozpiRozpisItemsByRiIdRodicArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<RozpisItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};

/** A condition to be used against `Rozpi` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type RozpiCondition = {
  /** Checks for equality with the object’s `rDatum` field. */
  rDatum?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `rId` field. */
  rId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `rKde` field. */
  rKde?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `rLock` field. */
  rLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `rTimestamp` field. */
  rTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `rTrener` field. */
  rTrener?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `rVisible` field. */
  rVisible?: InputMaybe<Scalars['Boolean']>;
};

/** An input for mutations affecting `Rozpi` */
export type RozpiInput = {
  rDatum: Scalars['Date'];
  rId?: InputMaybe<Scalars['BigInt']>;
  rKde: Scalars['String'];
  rLock?: InputMaybe<Scalars['Boolean']>;
  rTimestamp?: InputMaybe<Scalars['Datetime']>;
  rTrener: Scalars['BigInt'];
  rVisible?: InputMaybe<Scalars['Boolean']>;
};

/** Represents an update to a `Rozpi`. Fields that are set will be updated. */
export type RozpiPatch = {
  rDatum?: InputMaybe<Scalars['Date']>;
  rId?: InputMaybe<Scalars['BigInt']>;
  rKde?: InputMaybe<Scalars['String']>;
  rLock?: InputMaybe<Scalars['Boolean']>;
  rTimestamp?: InputMaybe<Scalars['Datetime']>;
  rTrener?: InputMaybe<Scalars['BigInt']>;
  rVisible?: InputMaybe<Scalars['Boolean']>;
};

/** A connection to a list of `Rozpi` values. */
export type RozpisConnection = {
  __typename?: 'RozpisConnection';
  /** A list of edges which contains the `Rozpi` and cursor to aid in pagination. */
  edges: Array<RozpisEdge>;
  /** A list of `Rozpi` objects. */
  nodes: Array<Rozpi>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Rozpi` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Rozpi` edge in the connection. */
export type RozpisEdge = {
  __typename?: 'RozpisEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Rozpi` at the end of the edge. */
  node: Rozpi;
};

export type RozpisItem = Node & {
  __typename?: 'RozpisItem';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `Pary` that is related to this `RozpisItem`. */
  paryByRiPartner?: Maybe<Pary>;
  riDo: Scalars['Time'];
  riId: Scalars['BigInt'];
  riIdRodic: Scalars['BigInt'];
  riLock: Scalars['Boolean'];
  riOd: Scalars['Time'];
  riPartner?: Maybe<Scalars['BigInt']>;
  /** Reads a single `Rozpi` that is related to this `RozpisItem`. */
  rozpiByRiIdRodic?: Maybe<Rozpi>;
};

/**
 * A condition to be used against `RozpisItem` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type RozpisItemCondition = {
  /** Checks for equality with the object’s `riDo` field. */
  riDo?: InputMaybe<Scalars['Time']>;
  /** Checks for equality with the object’s `riId` field. */
  riId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `riIdRodic` field. */
  riIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `riLock` field. */
  riLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `riOd` field. */
  riOd?: InputMaybe<Scalars['Time']>;
  /** Checks for equality with the object’s `riPartner` field. */
  riPartner?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `RozpisItem` */
export type RozpisItemInput = {
  riDo: Scalars['Time'];
  riId?: InputMaybe<Scalars['BigInt']>;
  riIdRodic: Scalars['BigInt'];
  riLock?: InputMaybe<Scalars['Boolean']>;
  riOd: Scalars['Time'];
  riPartner?: InputMaybe<Scalars['BigInt']>;
};

/** Represents an update to a `RozpisItem`. Fields that are set will be updated. */
export type RozpisItemPatch = {
  riDo?: InputMaybe<Scalars['Time']>;
  riId?: InputMaybe<Scalars['BigInt']>;
  riIdRodic?: InputMaybe<Scalars['BigInt']>;
  riLock?: InputMaybe<Scalars['Boolean']>;
  riOd?: InputMaybe<Scalars['Time']>;
  riPartner?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `RozpisItem` values. */
export type RozpisItemsConnection = {
  __typename?: 'RozpisItemsConnection';
  /** A list of edges which contains the `RozpisItem` and cursor to aid in pagination. */
  edges: Array<RozpisItemsEdge>;
  /** A list of `RozpisItem` objects. */
  nodes: Array<RozpisItem>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `RozpisItem` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `RozpisItem` edge in the connection. */
export type RozpisItemsEdge = {
  __typename?: 'RozpisItemsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `RozpisItem` at the end of the edge. */
  node: RozpisItem;
};

/** Methods to use when ordering `RozpisItem`. */
export enum RozpisItemsOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  RiDoAsc = 'RI_DO_ASC',
  RiDoDesc = 'RI_DO_DESC',
  RiIdAsc = 'RI_ID_ASC',
  RiIdDesc = 'RI_ID_DESC',
  RiIdRodicAsc = 'RI_ID_RODIC_ASC',
  RiIdRodicDesc = 'RI_ID_RODIC_DESC',
  RiLockAsc = 'RI_LOCK_ASC',
  RiLockDesc = 'RI_LOCK_DESC',
  RiOdAsc = 'RI_OD_ASC',
  RiOdDesc = 'RI_OD_DESC',
  RiPartnerAsc = 'RI_PARTNER_ASC',
  RiPartnerDesc = 'RI_PARTNER_DESC'
}

/** Methods to use when ordering `Rozpi`. */
export enum RozpisOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  RDatumAsc = 'R_DATUM_ASC',
  RDatumDesc = 'R_DATUM_DESC',
  RIdAsc = 'R_ID_ASC',
  RIdDesc = 'R_ID_DESC',
  RKdeAsc = 'R_KDE_ASC',
  RKdeDesc = 'R_KDE_DESC',
  RLockAsc = 'R_LOCK_ASC',
  RLockDesc = 'R_LOCK_DESC',
  RTimestampAsc = 'R_TIMESTAMP_ASC',
  RTimestampDesc = 'R_TIMESTAMP_DESC',
  RTrenerAsc = 'R_TRENER_ASC',
  RTrenerDesc = 'R_TRENER_DESC',
  RVisibleAsc = 'R_VISIBLE_ASC',
  RVisibleDesc = 'R_VISIBLE_DESC'
}

export type Session = Node & {
  __typename?: 'Session';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  ssData: Scalars['String'];
  ssId: Scalars['String'];
  ssLifetime: Scalars['BigInt'];
  ssUpdatedAt: Scalars['Datetime'];
  ssUser?: Maybe<Scalars['BigInt']>;
  /** Reads a single `User` that is related to this `Session`. */
  userBySsUser?: Maybe<User>;
};

/** A condition to be used against `Session` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type SessionCondition = {
  /** Checks for equality with the object’s `ssData` field. */
  ssData?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `ssId` field. */
  ssId?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `ssLifetime` field. */
  ssLifetime?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `ssUpdatedAt` field. */
  ssUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `ssUser` field. */
  ssUser?: InputMaybe<Scalars['BigInt']>;
};

/** An input for mutations affecting `Session` */
export type SessionInput = {
  ssData: Scalars['String'];
  ssId: Scalars['String'];
  ssLifetime: Scalars['BigInt'];
  ssUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  ssUser?: InputMaybe<Scalars['BigInt']>;
};

/** Represents an update to a `Session`. Fields that are set will be updated. */
export type SessionPatch = {
  ssData?: InputMaybe<Scalars['String']>;
  ssId?: InputMaybe<Scalars['String']>;
  ssLifetime?: InputMaybe<Scalars['BigInt']>;
  ssUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  ssUser?: InputMaybe<Scalars['BigInt']>;
};

/** A connection to a list of `Session` values. */
export type SessionsConnection = {
  __typename?: 'SessionsConnection';
  /** A list of edges which contains the `Session` and cursor to aid in pagination. */
  edges: Array<SessionsEdge>;
  /** A list of `Session` objects. */
  nodes: Array<Session>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Session` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Session` edge in the connection. */
export type SessionsEdge = {
  __typename?: 'SessionsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Session` at the end of the edge. */
  node: Session;
};

/** Methods to use when ordering `Session`. */
export enum SessionsOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  SsDataAsc = 'SS_DATA_ASC',
  SsDataDesc = 'SS_DATA_DESC',
  SsIdAsc = 'SS_ID_ASC',
  SsIdDesc = 'SS_ID_DESC',
  SsLifetimeAsc = 'SS_LIFETIME_ASC',
  SsLifetimeDesc = 'SS_LIFETIME_DESC',
  SsUpdatedAtAsc = 'SS_UPDATED_AT_ASC',
  SsUpdatedAtDesc = 'SS_UPDATED_AT_DESC',
  SsUserAsc = 'SS_USER_ASC',
  SsUserDesc = 'SS_USER_DESC'
}

/** A connection to a list of `Skupiny` values. */
export type SkupiniesConnection = {
  __typename?: 'SkupiniesConnection';
  /** A list of edges which contains the `Skupiny` and cursor to aid in pagination. */
  edges: Array<SkupiniesEdge>;
  /** A list of `Skupiny` objects. */
  nodes: Array<Skupiny>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Skupiny` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Skupiny` edge in the connection. */
export type SkupiniesEdge = {
  __typename?: 'SkupiniesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Skupiny` at the end of the edge. */
  node: Skupiny;
};

/** Methods to use when ordering `Skupiny`. */
export enum SkupiniesOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  SColorRgbAsc = 'S_COLOR_RGB_ASC',
  SColorRgbDesc = 'S_COLOR_RGB_DESC',
  SColorTextAsc = 'S_COLOR_TEXT_ASC',
  SColorTextDesc = 'S_COLOR_TEXT_DESC',
  SDescriptionAsc = 'S_DESCRIPTION_ASC',
  SDescriptionDesc = 'S_DESCRIPTION_DESC',
  SIdAsc = 'S_ID_ASC',
  SIdDesc = 'S_ID_DESC',
  SNameAsc = 'S_NAME_ASC',
  SNameDesc = 'S_NAME_DESC'
}

export type Skupiny = Node & {
  __typename?: 'Skupiny';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads and enables pagination through a set of `PlatbyGroupSkupina`. */
  platbyGroupSkupinasByPgsIdSkupina: PlatbyGroupSkupinasConnection;
  sColorRgb: Scalars['String'];
  sColorText: Scalars['String'];
  sDescription: Scalars['String'];
  sId: Scalars['BigInt'];
  sName: Scalars['String'];
  /** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
  upozorneniSkupiniesByUpsIdSkupina: UpozorneniSkupiniesConnection;
  /** Reads and enables pagination through a set of `User`. */
  usersByUSkupina: UsersConnection;
};


export type SkupinyPlatbyGroupSkupinasByPgsIdSkupinaArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyGroupSkupinaCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};


export type SkupinyUpozorneniSkupiniesByUpsIdSkupinaArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UpozorneniSkupinyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};


export type SkupinyUsersByUSkupinaArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UserCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};

/** A condition to be used against `Skupiny` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type SkupinyCondition = {
  /** Checks for equality with the object’s `sColorRgb` field. */
  sColorRgb?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `sColorText` field. */
  sColorText?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `sDescription` field. */
  sDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `sId` field. */
  sId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `sName` field. */
  sName?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `Skupiny` */
export type SkupinyInput = {
  sColorRgb: Scalars['String'];
  sColorText: Scalars['String'];
  sDescription: Scalars['String'];
  sId?: InputMaybe<Scalars['BigInt']>;
  sName: Scalars['String'];
};

/** Represents an update to a `Skupiny`. Fields that are set will be updated. */
export type SkupinyPatch = {
  sColorRgb?: InputMaybe<Scalars['String']>;
  sColorText?: InputMaybe<Scalars['String']>;
  sDescription?: InputMaybe<Scalars['String']>;
  sId?: InputMaybe<Scalars['BigInt']>;
  sName?: InputMaybe<Scalars['String']>;
};

/** All input for the `updateAkceByAId` mutation. */
export type UpdateAkceByAIdInput = {
  aId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Akce` being updated. */
  akcePatch: AkcePatch;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `updateAkce` mutation. */
export type UpdateAkceInput = {
  /** An object where the defined keys will be set on the `Akce` being updated. */
  akcePatch: AkcePatch;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Akce` to be updated. */
  nodeId: Scalars['ID'];
};

/** All input for the `updateAkceItemByAiId` mutation. */
export type UpdateAkceItemByAiIdInput = {
  aiId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `AkceItem` being updated. */
  akceItemPatch: AkceItemPatch;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `updateAkceItem` mutation. */
export type UpdateAkceItemInput = {
  /** An object where the defined keys will be set on the `AkceItem` being updated. */
  akceItemPatch: AkceItemPatch;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `AkceItem` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `AkceItem` mutation. */
export type UpdateAkceItemPayload = {
  __typename?: 'UpdateAkceItemPayload';
  /** Reads a single `Akce` that is related to this `AkceItem`. */
  akceByAiIdRodic?: Maybe<Akce>;
  /** The `AkceItem` that was updated by this mutation. */
  akceItem?: Maybe<AkceItem>;
  /** An edge for our `AkceItem`. May be used by Relay 1. */
  akceItemEdge?: Maybe<AkceItemsEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `AkceItem`. */
  userByAiUser?: Maybe<User>;
};


/** The output of our update `AkceItem` mutation. */
export type UpdateAkceItemPayloadAkceItemEdgeArgs = {
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};

/** The output of our update `Akce` mutation. */
export type UpdateAkcePayload = {
  __typename?: 'UpdateAkcePayload';
  /** The `Akce` that was updated by this mutation. */
  akce?: Maybe<Akce>;
  /** An edge for our `Akce`. May be used by Relay 1. */
  akceEdge?: Maybe<AkcesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `Akce` mutation. */
export type UpdateAkcePayloadAkceEdgeArgs = {
  orderBy?: InputMaybe<Array<AkcesOrderBy>>;
};

/** All input for the `updateAktualityByAtId` mutation. */
export type UpdateAktualityByAtIdInput = {
  /** An object where the defined keys will be set on the `Aktuality` being updated. */
  aktualityPatch: AktualityPatch;
  atId: Scalars['BigInt'];
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
};

/** All input for the `updateAktuality` mutation. */
export type UpdateAktualityInput = {
  /** An object where the defined keys will be set on the `Aktuality` being updated. */
  aktualityPatch: AktualityPatch;
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Aktuality` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `Aktuality` mutation. */
export type UpdateAktualityPayload = {
  __typename?: 'UpdateAktualityPayload';
  /** The `Aktuality` that was updated by this mutation. */
  aktuality?: Maybe<Aktuality>;
  /** An edge for our `Aktuality`. May be used by Relay 1. */
  aktualityEdge?: Maybe<AktualitiesEdge>;
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `GalerieFoto` that is related to this `Aktuality`. */
  galerieFotoByAtFotoMain?: Maybe<GalerieFoto>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Aktuality`. */
  userByAtKdo?: Maybe<User>;
};


/** The output of our update `Aktuality` mutation. */
export type UpdateAktualityPayloadAktualityEdgeArgs = {
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};

/** All input for the `updateDokumentyByDId` mutation. */
export type UpdateDokumentyByDIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  dId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Dokumenty` being updated. */
  dokumentyPatch: DokumentyPatch;
};

/** All input for the `updateDokumenty` mutation. */
export type UpdateDokumentyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `Dokumenty` being updated. */
  dokumentyPatch: DokumentyPatch;
  /** The globally unique `ID` which will identify a single `Dokumenty` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `Dokumenty` mutation. */
export type UpdateDokumentyPayload = {
  __typename?: 'UpdateDokumentyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Dokumenty` that was updated by this mutation. */
  dokumenty?: Maybe<Dokumenty>;
  /** An edge for our `Dokumenty`. May be used by Relay 1. */
  dokumentyEdge?: Maybe<DokumentiesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Dokumenty`. */
  userByDKdo?: Maybe<User>;
};


/** The output of our update `Dokumenty` mutation. */
export type UpdateDokumentyPayloadDokumentyEdgeArgs = {
  orderBy?: InputMaybe<Array<DokumentiesOrderBy>>;
};

/** All input for the `updateGalerieDirByGdId` mutation. */
export type UpdateGalerieDirByGdIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `GalerieDir` being updated. */
  galerieDirPatch: GalerieDirPatch;
  gdId: Scalars['BigInt'];
};

/** All input for the `updateGalerieDir` mutation. */
export type UpdateGalerieDirInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `GalerieDir` being updated. */
  galerieDirPatch: GalerieDirPatch;
  /** The globally unique `ID` which will identify a single `GalerieDir` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `GalerieDir` mutation. */
export type UpdateGalerieDirPayload = {
  __typename?: 'UpdateGalerieDirPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `GalerieDir` that was updated by this mutation. */
  galerieDir?: Maybe<GalerieDir>;
  /** An edge for our `GalerieDir`. May be used by Relay 1. */
  galerieDirEdge?: Maybe<GalerieDirsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `GalerieDir` mutation. */
export type UpdateGalerieDirPayloadGalerieDirEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieDirsOrderBy>>;
};

/** All input for the `updateGalerieFotoByGfId` mutation. */
export type UpdateGalerieFotoByGfIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `GalerieFoto` being updated. */
  galerieFotoPatch: GalerieFotoPatch;
  gfId: Scalars['BigInt'];
};

/** All input for the `updateGalerieFoto` mutation. */
export type UpdateGalerieFotoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `GalerieFoto` being updated. */
  galerieFotoPatch: GalerieFotoPatch;
  /** The globally unique `ID` which will identify a single `GalerieFoto` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `GalerieFoto` mutation. */
export type UpdateGalerieFotoPayload = {
  __typename?: 'UpdateGalerieFotoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `GalerieDir` that is related to this `GalerieFoto`. */
  galerieDirByGfIdRodic?: Maybe<GalerieDir>;
  /** The `GalerieFoto` that was updated by this mutation. */
  galerieFoto?: Maybe<GalerieFoto>;
  /** An edge for our `GalerieFoto`. May be used by Relay 1. */
  galerieFotoEdge?: Maybe<GalerieFotosEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `GalerieFoto`. */
  userByGfKdo?: Maybe<User>;
};


/** The output of our update `GalerieFoto` mutation. */
export type UpdateGalerieFotoPayloadGalerieFotoEdgeArgs = {
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};

/** All input for the `updateNabidkaByNId` mutation. */
export type UpdateNabidkaByNIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  nId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Nabidka` being updated. */
  nabidkaPatch: NabidkaPatch;
};

/** All input for the `updateNabidka` mutation. */
export type UpdateNabidkaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `Nabidka` being updated. */
  nabidkaPatch: NabidkaPatch;
  /** The globally unique `ID` which will identify a single `Nabidka` to be updated. */
  nodeId: Scalars['ID'];
};

/** All input for the `updateNabidkaItemByNiId` mutation. */
export type UpdateNabidkaItemByNiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `NabidkaItem` being updated. */
  nabidkaItemPatch: NabidkaItemPatch;
  niId: Scalars['BigInt'];
};

/** All input for the `updateNabidkaItem` mutation. */
export type UpdateNabidkaItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `NabidkaItem` being updated. */
  nabidkaItemPatch: NabidkaItemPatch;
  /** The globally unique `ID` which will identify a single `NabidkaItem` to be updated. */
  nodeId: Scalars['ID'];
};

/** The output of our update `NabidkaItem` mutation. */
export type UpdateNabidkaItemPayload = {
  __typename?: 'UpdateNabidkaItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Nabidka` that is related to this `NabidkaItem`. */
  nabidkaByNiIdRodic?: Maybe<Nabidka>;
  /** The `NabidkaItem` that was updated by this mutation. */
  nabidkaItem?: Maybe<NabidkaItem>;
  /** An edge for our `NabidkaItem`. May be used by Relay 1. */
  nabidkaItemEdge?: Maybe<NabidkaItemsEdge>;
  /** Reads a single `Pary` that is related to this `NabidkaItem`. */
  paryByNiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `NabidkaItem` mutation. */
export type UpdateNabidkaItemPayloadNabidkaItemEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkaItemsOrderBy>>;
};

/** The output of our update `Nabidka` mutation. */
export type UpdateNabidkaPayload = {
  __typename?: 'UpdateNabidkaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Nabidka` that was updated by this mutation. */
  nabidka?: Maybe<Nabidka>;
  /** An edge for our `Nabidka`. May be used by Relay 1. */
  nabidkaEdge?: Maybe<NabidkasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Nabidka`. */
  userByNTrener?: Maybe<User>;
};


/** The output of our update `Nabidka` mutation. */
export type UpdateNabidkaPayloadNabidkaEdgeArgs = {
  orderBy?: InputMaybe<Array<NabidkasOrderBy>>;
};

/** All input for the `updatePageById` mutation. */
export type UpdatePageByIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  id: Scalars['Int'];
  /** An object where the defined keys will be set on the `Page` being updated. */
  pagePatch: PagePatch;
};

/** All input for the `updatePageByUrl` mutation. */
export type UpdatePageByUrlInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `Page` being updated. */
  pagePatch: PagePatch;
  url: Scalars['String'];
};

/** All input for the `updatePage` mutation. */
export type UpdatePageInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Page` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Page` being updated. */
  pagePatch: PagePatch;
};

/** The output of our update `Page` mutation. */
export type UpdatePagePayload = {
  __typename?: 'UpdatePagePayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Page` that was updated by this mutation. */
  page?: Maybe<Page>;
  /** An edge for our `Page`. May be used by Relay 1. */
  pageEdge?: Maybe<PagesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `Page` mutation. */
export type UpdatePagePayloadPageEdgeArgs = {
  orderBy?: InputMaybe<Array<PagesOrderBy>>;
};

/** All input for the `updateParameterByPaName` mutation. */
export type UpdateParameterByPaNameInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  paName: Scalars['String'];
  /** An object where the defined keys will be set on the `Parameter` being updated. */
  parameterPatch: ParameterPatch;
};

/** All input for the `updateParameter` mutation. */
export type UpdateParameterInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Parameter` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Parameter` being updated. */
  parameterPatch: ParameterPatch;
};

/** The output of our update `Parameter` mutation. */
export type UpdateParameterPayload = {
  __typename?: 'UpdateParameterPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Parameter` that was updated by this mutation. */
  parameter?: Maybe<Parameter>;
  /** An edge for our `Parameter`. May be used by Relay 1. */
  parameterEdge?: Maybe<ParametersEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `Parameter` mutation. */
export type UpdateParameterPayloadParameterEdgeArgs = {
  orderBy?: InputMaybe<Array<ParametersOrderBy>>;
};

/** All input for the `updateParyByPId` mutation. */
export type UpdateParyByPIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Pary` being updated. */
  paryPatch: ParyPatch;
};

/** All input for the `updatePary` mutation. */
export type UpdateParyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Pary` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Pary` being updated. */
  paryPatch: ParyPatch;
};

/** All input for the `updateParyNavrhByPnId` mutation. */
export type UpdateParyNavrhByPnIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `ParyNavrh` being updated. */
  paryNavrhPatch: ParyNavrhPatch;
  pnId: Scalars['BigInt'];
};

/** All input for the `updateParyNavrh` mutation. */
export type UpdateParyNavrhInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `ParyNavrh` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `ParyNavrh` being updated. */
  paryNavrhPatch: ParyNavrhPatch;
};

/** The output of our update `ParyNavrh` mutation. */
export type UpdateParyNavrhPayload = {
  __typename?: 'UpdateParyNavrhPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `ParyNavrh` that was updated by this mutation. */
  paryNavrh?: Maybe<ParyNavrh>;
  /** An edge for our `ParyNavrh`. May be used by Relay 1. */
  paryNavrhEdge?: Maybe<ParyNavrhsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnNavrhl?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartner?: Maybe<User>;
  /** Reads a single `User` that is related to this `ParyNavrh`. */
  userByPnPartnerka?: Maybe<User>;
};


/** The output of our update `ParyNavrh` mutation. */
export type UpdateParyNavrhPayloadParyNavrhEdgeArgs = {
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};

/** The output of our update `Pary` mutation. */
export type UpdateParyPayload = {
  __typename?: 'UpdateParyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Pary` that was updated by this mutation. */
  pary?: Maybe<Pary>;
  /** An edge for our `Pary`. May be used by Relay 1. */
  paryEdge?: Maybe<PariesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `Pary`. */
  userByPIdPartner?: Maybe<User>;
};


/** The output of our update `Pary` mutation. */
export type UpdateParyPayloadParyEdgeArgs = {
  orderBy?: InputMaybe<Array<PariesOrderBy>>;
};

/** All input for the `updatePermissionByPeId` mutation. */
export type UpdatePermissionByPeIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  peId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Permission` being updated. */
  permissionPatch: PermissionPatch;
};

/** All input for the `updatePermission` mutation. */
export type UpdatePermissionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Permission` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Permission` being updated. */
  permissionPatch: PermissionPatch;
};

/** The output of our update `Permission` mutation. */
export type UpdatePermissionPayload = {
  __typename?: 'UpdatePermissionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `Permission` that was updated by this mutation. */
  permission?: Maybe<Permission>;
  /** An edge for our `Permission`. May be used by Relay 1. */
  permissionEdge?: Maybe<PermissionsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `Permission` mutation. */
export type UpdatePermissionPayloadPermissionEdgeArgs = {
  orderBy?: InputMaybe<Array<PermissionsOrderBy>>;
};

/** All input for the `updatePlatbyCategoryByPcId` mutation. */
export type UpdatePlatbyCategoryByPcIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pcId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
  platbyCategoryPatch: PlatbyCategoryPatch;
};

/** All input for the `updatePlatbyCategoryGroupByPcgId` mutation. */
export type UpdatePlatbyCategoryGroupByPcgIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pcgId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
  platbyCategoryGroupPatch: PlatbyCategoryGroupPatch;
};

/** All input for the `updatePlatbyCategoryGroup` mutation. */
export type UpdatePlatbyCategoryGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyCategoryGroup` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyCategoryGroup` being updated. */
  platbyCategoryGroupPatch: PlatbyCategoryGroupPatch;
};

/** The output of our update `PlatbyCategoryGroup` mutation. */
export type UpdatePlatbyCategoryGroupPayload = {
  __typename?: 'UpdatePlatbyCategoryGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyCategoryGroup`. */
  platbyCategoryByPcgIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyCategoryGroup` that was updated by this mutation. */
  platbyCategoryGroup?: Maybe<PlatbyCategoryGroup>;
  /** An edge for our `PlatbyCategoryGroup`. May be used by Relay 1. */
  platbyCategoryGroupEdge?: Maybe<PlatbyCategoryGroupsEdge>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyCategoryGroup`. */
  platbyGroupByPcgIdGroup?: Maybe<PlatbyGroup>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `PlatbyCategoryGroup` mutation. */
export type UpdatePlatbyCategoryGroupPayloadPlatbyCategoryGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoryGroupsOrderBy>>;
};

/** All input for the `updatePlatbyCategory` mutation. */
export type UpdatePlatbyCategoryInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyCategory` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyCategory` being updated. */
  platbyCategoryPatch: PlatbyCategoryPatch;
};

/** The output of our update `PlatbyCategory` mutation. */
export type UpdatePlatbyCategoryPayload = {
  __typename?: 'UpdatePlatbyCategoryPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyCategory` that was updated by this mutation. */
  platbyCategory?: Maybe<PlatbyCategory>;
  /** An edge for our `PlatbyCategory`. May be used by Relay 1. */
  platbyCategoryEdge?: Maybe<PlatbyCategoriesEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `PlatbyCategory` mutation. */
export type UpdatePlatbyCategoryPayloadPlatbyCategoryEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyCategoriesOrderBy>>;
};

/** All input for the `updatePlatbyGroupByPgId` mutation. */
export type UpdatePlatbyGroupByPgIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pgId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
  platbyGroupPatch: PlatbyGroupPatch;
};

/** All input for the `updatePlatbyGroup` mutation. */
export type UpdatePlatbyGroupInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyGroup` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyGroup` being updated. */
  platbyGroupPatch: PlatbyGroupPatch;
};

/** The output of our update `PlatbyGroup` mutation. */
export type UpdatePlatbyGroupPayload = {
  __typename?: 'UpdatePlatbyGroupPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyGroup` that was updated by this mutation. */
  platbyGroup?: Maybe<PlatbyGroup>;
  /** An edge for our `PlatbyGroup`. May be used by Relay 1. */
  platbyGroupEdge?: Maybe<PlatbyGroupsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `PlatbyGroup` mutation. */
export type UpdatePlatbyGroupPayloadPlatbyGroupEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupsOrderBy>>;
};

/** All input for the `updatePlatbyGroupSkupinaByPgsId` mutation. */
export type UpdatePlatbyGroupSkupinaByPgsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  pgsId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
  platbyGroupSkupinaPatch: PlatbyGroupSkupinaPatch;
};

/** All input for the `updatePlatbyGroupSkupina` mutation. */
export type UpdatePlatbyGroupSkupinaInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyGroupSkupina` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyGroupSkupina` being updated. */
  platbyGroupSkupinaPatch: PlatbyGroupSkupinaPatch;
};

/** The output of our update `PlatbyGroupSkupina` mutation. */
export type UpdatePlatbyGroupSkupinaPayload = {
  __typename?: 'UpdatePlatbyGroupSkupinaPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyGroup` that is related to this `PlatbyGroupSkupina`. */
  platbyGroupByPgsIdGroup?: Maybe<PlatbyGroup>;
  /** The `PlatbyGroupSkupina` that was updated by this mutation. */
  platbyGroupSkupina?: Maybe<PlatbyGroupSkupina>;
  /** An edge for our `PlatbyGroupSkupina`. May be used by Relay 1. */
  platbyGroupSkupinaEdge?: Maybe<PlatbyGroupSkupinasEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `PlatbyGroupSkupina`. */
  skupinyByPgsIdSkupina?: Maybe<Skupiny>;
};


/** The output of our update `PlatbyGroupSkupina` mutation. */
export type UpdatePlatbyGroupSkupinaPayloadPlatbyGroupSkupinaEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyGroupSkupinasOrderBy>>;
};

/** All input for the `updatePlatbyItemByPiId` mutation. */
export type UpdatePlatbyItemByPiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  piId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `PlatbyItem` being updated. */
  platbyItemPatch: PlatbyItemPatch;
};

/** All input for the `updatePlatbyItem` mutation. */
export type UpdatePlatbyItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyItem` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyItem` being updated. */
  platbyItemPatch: PlatbyItemPatch;
};

/** The output of our update `PlatbyItem` mutation. */
export type UpdatePlatbyItemPayload = {
  __typename?: 'UpdatePlatbyItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `PlatbyCategory` that is related to this `PlatbyItem`. */
  platbyCategoryByPiIdCategory?: Maybe<PlatbyCategory>;
  /** The `PlatbyItem` that was updated by this mutation. */
  platbyItem?: Maybe<PlatbyItem>;
  /** An edge for our `PlatbyItem`. May be used by Relay 1. */
  platbyItemEdge?: Maybe<PlatbyItemsEdge>;
  /** Reads a single `PlatbyRaw` that is related to this `PlatbyItem`. */
  platbyRawByPiIdRaw?: Maybe<PlatbyRaw>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `User` that is related to this `PlatbyItem`. */
  userByPiIdUser?: Maybe<User>;
};


/** The output of our update `PlatbyItem` mutation. */
export type UpdatePlatbyItemPayloadPlatbyItemEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};

/** All input for the `updatePlatbyRawByPrId` mutation. */
export type UpdatePlatbyRawByPrIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
  platbyRawPatch: PlatbyRawPatch;
  prId: Scalars['BigInt'];
};

/** All input for the `updatePlatbyRaw` mutation. */
export type UpdatePlatbyRawInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `PlatbyRaw` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `PlatbyRaw` being updated. */
  platbyRawPatch: PlatbyRawPatch;
};

/** The output of our update `PlatbyRaw` mutation. */
export type UpdatePlatbyRawPayload = {
  __typename?: 'UpdatePlatbyRawPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** The `PlatbyRaw` that was updated by this mutation. */
  platbyRaw?: Maybe<PlatbyRaw>;
  /** An edge for our `PlatbyRaw`. May be used by Relay 1. */
  platbyRawEdge?: Maybe<PlatbyRawsEdge>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
};


/** The output of our update `PlatbyRaw` mutation. */
export type UpdatePlatbyRawPayloadPlatbyRawEdgeArgs = {
  orderBy?: InputMaybe<Array<PlatbyRawsOrderBy>>;
};

/** All input for the `updateRozpiByRId` mutation. */
export type UpdateRozpiByRIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  rId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Rozpi` being updated. */
  rozpiPatch: RozpiPatch;
};

/** All input for the `updateRozpi` mutation. */
export type UpdateRozpiInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Rozpi` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Rozpi` being updated. */
  rozpiPatch: RozpiPatch;
};

/** The output of our update `Rozpi` mutation. */
export type UpdateRozpiPayload = {
  __typename?: 'UpdateRozpiPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Rozpi` that was updated by this mutation. */
  rozpi?: Maybe<Rozpi>;
  /** An edge for our `Rozpi`. May be used by Relay 1. */
  rozpiEdge?: Maybe<RozpisEdge>;
  /** Reads a single `User` that is related to this `Rozpi`. */
  userByRTrener?: Maybe<User>;
};


/** The output of our update `Rozpi` mutation. */
export type UpdateRozpiPayloadRozpiEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisOrderBy>>;
};

/** All input for the `updateRozpisItemByRiId` mutation. */
export type UpdateRozpisItemByRiIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  riId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `RozpisItem` being updated. */
  rozpisItemPatch: RozpisItemPatch;
};

/** All input for the `updateRozpisItem` mutation. */
export type UpdateRozpisItemInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `RozpisItem` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `RozpisItem` being updated. */
  rozpisItemPatch: RozpisItemPatch;
};

/** The output of our update `RozpisItem` mutation. */
export type UpdateRozpisItemPayload = {
  __typename?: 'UpdateRozpisItemPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Pary` that is related to this `RozpisItem`. */
  paryByRiPartner?: Maybe<Pary>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Rozpi` that is related to this `RozpisItem`. */
  rozpiByRiIdRodic?: Maybe<Rozpi>;
  /** The `RozpisItem` that was updated by this mutation. */
  rozpisItem?: Maybe<RozpisItem>;
  /** An edge for our `RozpisItem`. May be used by Relay 1. */
  rozpisItemEdge?: Maybe<RozpisItemsEdge>;
};


/** The output of our update `RozpisItem` mutation. */
export type UpdateRozpisItemPayloadRozpisItemEdgeArgs = {
  orderBy?: InputMaybe<Array<RozpisItemsOrderBy>>;
};

/** All input for the `updateSessionBySsId` mutation. */
export type UpdateSessionBySsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `Session` being updated. */
  sessionPatch: SessionPatch;
  ssId: Scalars['String'];
};

/** All input for the `updateSession` mutation. */
export type UpdateSessionInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Session` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Session` being updated. */
  sessionPatch: SessionPatch;
};

/** The output of our update `Session` mutation. */
export type UpdateSessionPayload = {
  __typename?: 'UpdateSessionPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Session` that was updated by this mutation. */
  session?: Maybe<Session>;
  /** An edge for our `Session`. May be used by Relay 1. */
  sessionEdge?: Maybe<SessionsEdge>;
  /** Reads a single `User` that is related to this `Session`. */
  userBySsUser?: Maybe<User>;
};


/** The output of our update `Session` mutation. */
export type UpdateSessionPayloadSessionEdgeArgs = {
  orderBy?: InputMaybe<Array<SessionsOrderBy>>;
};

/** All input for the `updateSkupinyBySId` mutation. */
export type UpdateSkupinyBySIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  sId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Skupiny` being updated. */
  skupinyPatch: SkupinyPatch;
};

/** All input for the `updateSkupiny` mutation. */
export type UpdateSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Skupiny` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Skupiny` being updated. */
  skupinyPatch: SkupinyPatch;
};

/** The output of our update `Skupiny` mutation. */
export type UpdateSkupinyPayload = {
  __typename?: 'UpdateSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Skupiny` that was updated by this mutation. */
  skupiny?: Maybe<Skupiny>;
  /** An edge for our `Skupiny`. May be used by Relay 1. */
  skupinyEdge?: Maybe<SkupiniesEdge>;
};


/** The output of our update `Skupiny` mutation. */
export type UpdateSkupinyPayloadSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<SkupiniesOrderBy>>;
};

/** All input for the `updateUpozorneniByUpId` mutation. */
export type UpdateUpozorneniByUpIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  upId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Upozorneni` being updated. */
  upozorneniPatch: UpozorneniPatch;
};

/** All input for the `updateUpozorneni` mutation. */
export type UpdateUpozorneniInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Upozorneni` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Upozorneni` being updated. */
  upozorneniPatch: UpozorneniPatch;
};

/** The output of our update `Upozorneni` mutation. */
export type UpdateUpozorneniPayload = {
  __typename?: 'UpdateUpozorneniPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Upozorneni` that was updated by this mutation. */
  upozorneni?: Maybe<Upozorneni>;
  /** An edge for our `Upozorneni`. May be used by Relay 1. */
  upozorneniEdge?: Maybe<UpozornenisEdge>;
  /** Reads a single `User` that is related to this `Upozorneni`. */
  userByUpKdo?: Maybe<User>;
};


/** The output of our update `Upozorneni` mutation. */
export type UpdateUpozorneniPayloadUpozorneniEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozornenisOrderBy>>;
};

/** All input for the `updateUpozorneniSkupinyByUpsId` mutation. */
export type UpdateUpozorneniSkupinyByUpsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
  upozorneniSkupinyPatch: UpozorneniSkupinyPatch;
  upsId: Scalars['BigInt'];
};

/** All input for the `updateUpozorneniSkupiny` mutation. */
export type UpdateUpozorneniSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `UpozorneniSkupiny` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `UpozorneniSkupiny` being updated. */
  upozorneniSkupinyPatch: UpozorneniSkupinyPatch;
};

/** The output of our update `UpozorneniSkupiny` mutation. */
export type UpdateUpozorneniSkupinyPayload = {
  __typename?: 'UpdateUpozorneniSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
  skupinyByUpsIdSkupina?: Maybe<Skupiny>;
  /** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
  upozorneniByUpsIdRodic?: Maybe<Upozorneni>;
  /** The `UpozorneniSkupiny` that was updated by this mutation. */
  upozorneniSkupiny?: Maybe<UpozorneniSkupiny>;
  /** An edge for our `UpozorneniSkupiny`. May be used by Relay 1. */
  upozorneniSkupinyEdge?: Maybe<UpozorneniSkupiniesEdge>;
};


/** The output of our update `UpozorneniSkupiny` mutation. */
export type UpdateUpozorneniSkupinyPayloadUpozorneniSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};

/** All input for the `updateUserByUId` mutation. */
export type UpdateUserByUIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  uId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `User` being updated. */
  userPatch: UserPatch;
};

/** All input for the `updateUser` mutation. */
export type UpdateUserInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `User` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `User` being updated. */
  userPatch: UserPatch;
};

/** The output of our update `User` mutation. */
export type UpdateUserPayload = {
  __typename?: 'UpdateUserPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Reads a single `Permission` that is related to this `User`. */
  permissionByUGroup?: Maybe<Permission>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** Reads a single `Skupiny` that is related to this `User`. */
  skupinyByUSkupina?: Maybe<Skupiny>;
  /** The `User` that was updated by this mutation. */
  user?: Maybe<User>;
  /** An edge for our `User`. May be used by Relay 1. */
  userEdge?: Maybe<UsersEdge>;
};


/** The output of our update `User` mutation. */
export type UpdateUserPayloadUserEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersOrderBy>>;
};

/** All input for the `updateUsersSkupinyByUsId` mutation. */
export type UpdateUsersSkupinyByUsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  usId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
  usersSkupinyPatch: UsersSkupinyPatch;
};

/** All input for the `updateUsersSkupiny` mutation. */
export type UpdateUsersSkupinyInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `UsersSkupiny` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `UsersSkupiny` being updated. */
  usersSkupinyPatch: UsersSkupinyPatch;
};

/** The output of our update `UsersSkupiny` mutation. */
export type UpdateUsersSkupinyPayload = {
  __typename?: 'UpdateUsersSkupinyPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `UsersSkupiny` that was updated by this mutation. */
  usersSkupiny?: Maybe<UsersSkupiny>;
  /** An edge for our `UsersSkupiny`. May be used by Relay 1. */
  usersSkupinyEdge?: Maybe<UsersSkupiniesEdge>;
};


/** The output of our update `UsersSkupiny` mutation. */
export type UpdateUsersSkupinyPayloadUsersSkupinyEdgeArgs = {
  orderBy?: InputMaybe<Array<UsersSkupiniesOrderBy>>;
};

/** All input for the `updateVideoByVId` mutation. */
export type UpdateVideoByVIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  vId: Scalars['BigInt'];
  /** An object where the defined keys will be set on the `Video` being updated. */
  videoPatch: VideoPatch;
};

/** All input for the `updateVideo` mutation. */
export type UpdateVideoInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `Video` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `Video` being updated. */
  videoPatch: VideoPatch;
};

/** All input for the `updateVideoListByVlId` mutation. */
export type UpdateVideoListByVlIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `VideoList` being updated. */
  videoListPatch: VideoListPatch;
  vlId: Scalars['BigInt'];
};

/** All input for the `updateVideoList` mutation. */
export type UpdateVideoListInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `VideoList` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `VideoList` being updated. */
  videoListPatch: VideoListPatch;
};

/** The output of our update `VideoList` mutation. */
export type UpdateVideoListPayload = {
  __typename?: 'UpdateVideoListPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoList` that was updated by this mutation. */
  videoList?: Maybe<VideoList>;
  /** An edge for our `VideoList`. May be used by Relay 1. */
  videoListEdge?: Maybe<VideoListsEdge>;
};


/** The output of our update `VideoList` mutation. */
export type UpdateVideoListPayloadVideoListEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoListsOrderBy>>;
};

/** The output of our update `Video` mutation. */
export type UpdateVideoPayload = {
  __typename?: 'UpdateVideoPayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `Video` that was updated by this mutation. */
  video?: Maybe<Video>;
  /** An edge for our `Video`. May be used by Relay 1. */
  videoEdge?: Maybe<VideosEdge>;
};


/** The output of our update `Video` mutation. */
export type UpdateVideoPayloadVideoEdgeArgs = {
  orderBy?: InputMaybe<Array<VideosOrderBy>>;
};

/** All input for the `updateVideoSourceByVsId` mutation. */
export type UpdateVideoSourceByVsIdInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** An object where the defined keys will be set on the `VideoSource` being updated. */
  videoSourcePatch: VideoSourcePatch;
  vsId: Scalars['BigInt'];
};

/** All input for the `updateVideoSource` mutation. */
export type UpdateVideoSourceInput = {
  /**
   * An arbitrary string value with no semantic meaning. Will be included in the
   * payload verbatim. May be used to track mutations by the client.
   */
  clientMutationId?: InputMaybe<Scalars['String']>;
  /** The globally unique `ID` which will identify a single `VideoSource` to be updated. */
  nodeId: Scalars['ID'];
  /** An object where the defined keys will be set on the `VideoSource` being updated. */
  videoSourcePatch: VideoSourcePatch;
};

/** The output of our update `VideoSource` mutation. */
export type UpdateVideoSourcePayload = {
  __typename?: 'UpdateVideoSourcePayload';
  /**
   * The exact same `clientMutationId` that was provided in the mutation input,
   * unchanged and unused. May be used by a client to track mutations.
   */
  clientMutationId?: Maybe<Scalars['String']>;
  /** Our root query field type. Allows us to run any query from our mutation payload. */
  query?: Maybe<Query>;
  /** The `VideoSource` that was updated by this mutation. */
  videoSource?: Maybe<VideoSource>;
  /** An edge for our `VideoSource`. May be used by Relay 1. */
  videoSourceEdge?: Maybe<VideoSourcesEdge>;
};


/** The output of our update `VideoSource` mutation. */
export type UpdateVideoSourcePayloadVideoSourceEdgeArgs = {
  orderBy?: InputMaybe<Array<VideoSourcesOrderBy>>;
};

export type Upozorneni = Node & {
  __typename?: 'Upozorneni';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  upBarvy: Scalars['BigInt'];
  upId: Scalars['BigInt'];
  upKdo: Scalars['BigInt'];
  upLock: Scalars['Boolean'];
  upNadpis: Scalars['String'];
  upText: Scalars['String'];
  upTimestamp?: Maybe<Scalars['Datetime']>;
  upTimestampAdd: Scalars['Datetime'];
  /** Reads and enables pagination through a set of `UpozorneniSkupiny`. */
  upozorneniSkupiniesByUpsIdRodic: UpozorneniSkupiniesConnection;
  /** Reads a single `User` that is related to this `Upozorneni`. */
  userByUpKdo?: Maybe<User>;
};


export type UpozorneniUpozorneniSkupiniesByUpsIdRodicArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UpozorneniSkupinyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UpozorneniSkupiniesOrderBy>>;
};

/**
 * A condition to be used against `Upozorneni` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type UpozorneniCondition = {
  /** Checks for equality with the object’s `upBarvy` field. */
  upBarvy?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upId` field. */
  upId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upKdo` field. */
  upKdo?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upLock` field. */
  upLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `upNadpis` field. */
  upNadpis?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `upText` field. */
  upText?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `upTimestamp` field. */
  upTimestamp?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `upTimestampAdd` field. */
  upTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `Upozorneni` */
export type UpozorneniInput = {
  upBarvy?: InputMaybe<Scalars['BigInt']>;
  upId?: InputMaybe<Scalars['BigInt']>;
  upKdo: Scalars['BigInt'];
  upLock?: InputMaybe<Scalars['Boolean']>;
  upNadpis: Scalars['String'];
  upText: Scalars['String'];
  upTimestamp?: InputMaybe<Scalars['Datetime']>;
  upTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** Represents an update to a `Upozorneni`. Fields that are set will be updated. */
export type UpozorneniPatch = {
  upBarvy?: InputMaybe<Scalars['BigInt']>;
  upId?: InputMaybe<Scalars['BigInt']>;
  upKdo?: InputMaybe<Scalars['BigInt']>;
  upLock?: InputMaybe<Scalars['Boolean']>;
  upNadpis?: InputMaybe<Scalars['String']>;
  upText?: InputMaybe<Scalars['String']>;
  upTimestamp?: InputMaybe<Scalars['Datetime']>;
  upTimestampAdd?: InputMaybe<Scalars['Datetime']>;
};

/** A connection to a list of `UpozorneniSkupiny` values. */
export type UpozorneniSkupiniesConnection = {
  __typename?: 'UpozorneniSkupiniesConnection';
  /** A list of edges which contains the `UpozorneniSkupiny` and cursor to aid in pagination. */
  edges: Array<UpozorneniSkupiniesEdge>;
  /** A list of `UpozorneniSkupiny` objects. */
  nodes: Array<UpozorneniSkupiny>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `UpozorneniSkupiny` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `UpozorneniSkupiny` edge in the connection. */
export type UpozorneniSkupiniesEdge = {
  __typename?: 'UpozorneniSkupiniesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `UpozorneniSkupiny` at the end of the edge. */
  node: UpozorneniSkupiny;
};

/** Methods to use when ordering `UpozorneniSkupiny`. */
export enum UpozorneniSkupiniesOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  UpsColorAsc = 'UPS_COLOR_ASC',
  UpsColorDesc = 'UPS_COLOR_DESC',
  UpsIdAsc = 'UPS_ID_ASC',
  UpsIdDesc = 'UPS_ID_DESC',
  UpsIdRodicAsc = 'UPS_ID_RODIC_ASC',
  UpsIdRodicDesc = 'UPS_ID_RODIC_DESC',
  UpsIdSkupinaAsc = 'UPS_ID_SKUPINA_ASC',
  UpsIdSkupinaDesc = 'UPS_ID_SKUPINA_DESC',
  UpsPopisAsc = 'UPS_POPIS_ASC',
  UpsPopisDesc = 'UPS_POPIS_DESC'
}

export type UpozorneniSkupiny = Node & {
  __typename?: 'UpozorneniSkupiny';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads a single `Skupiny` that is related to this `UpozorneniSkupiny`. */
  skupinyByUpsIdSkupina?: Maybe<Skupiny>;
  /** Reads a single `Upozorneni` that is related to this `UpozorneniSkupiny`. */
  upozorneniByUpsIdRodic?: Maybe<Upozorneni>;
  upsColor: Scalars['String'];
  upsId: Scalars['BigInt'];
  upsIdRodic: Scalars['BigInt'];
  upsIdSkupina: Scalars['BigInt'];
  upsPopis: Scalars['String'];
};

/**
 * A condition to be used against `UpozorneniSkupiny` object types. All fields are
 * tested for equality and combined with a logical ‘and.’
 */
export type UpozorneniSkupinyCondition = {
  /** Checks for equality with the object’s `upsColor` field. */
  upsColor?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `upsId` field. */
  upsId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upsIdRodic` field. */
  upsIdRodic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upsIdSkupina` field. */
  upsIdSkupina?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `upsPopis` field. */
  upsPopis?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `UpozorneniSkupiny` */
export type UpozorneniSkupinyInput = {
  upsColor: Scalars['String'];
  upsId?: InputMaybe<Scalars['BigInt']>;
  upsIdRodic: Scalars['BigInt'];
  upsIdSkupina: Scalars['BigInt'];
  upsPopis: Scalars['String'];
};

/** Represents an update to a `UpozorneniSkupiny`. Fields that are set will be updated. */
export type UpozorneniSkupinyPatch = {
  upsColor?: InputMaybe<Scalars['String']>;
  upsId?: InputMaybe<Scalars['BigInt']>;
  upsIdRodic?: InputMaybe<Scalars['BigInt']>;
  upsIdSkupina?: InputMaybe<Scalars['BigInt']>;
  upsPopis?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `Upozorneni` values. */
export type UpozornenisConnection = {
  __typename?: 'UpozornenisConnection';
  /** A list of edges which contains the `Upozorneni` and cursor to aid in pagination. */
  edges: Array<UpozornenisEdge>;
  /** A list of `Upozorneni` objects. */
  nodes: Array<Upozorneni>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Upozorneni` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Upozorneni` edge in the connection. */
export type UpozornenisEdge = {
  __typename?: 'UpozornenisEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Upozorneni` at the end of the edge. */
  node: Upozorneni;
};

/** Methods to use when ordering `Upozorneni`. */
export enum UpozornenisOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  UpBarvyAsc = 'UP_BARVY_ASC',
  UpBarvyDesc = 'UP_BARVY_DESC',
  UpIdAsc = 'UP_ID_ASC',
  UpIdDesc = 'UP_ID_DESC',
  UpKdoAsc = 'UP_KDO_ASC',
  UpKdoDesc = 'UP_KDO_DESC',
  UpLockAsc = 'UP_LOCK_ASC',
  UpLockDesc = 'UP_LOCK_DESC',
  UpNadpisAsc = 'UP_NADPIS_ASC',
  UpNadpisDesc = 'UP_NADPIS_DESC',
  UpTextAsc = 'UP_TEXT_ASC',
  UpTextDesc = 'UP_TEXT_DESC',
  UpTimestampAddAsc = 'UP_TIMESTAMP_ADD_ASC',
  UpTimestampAddDesc = 'UP_TIMESTAMP_ADD_DESC',
  UpTimestampAsc = 'UP_TIMESTAMP_ASC',
  UpTimestampDesc = 'UP_TIMESTAMP_DESC'
}

export type User = Node & {
  __typename?: 'User';
  /** Reads and enables pagination through a set of `AkceItem`. */
  akceItemsByAiUser: AkceItemsConnection;
  /** Reads and enables pagination through a set of `Aktuality`. */
  aktualitiesByAtKdo: AktualitiesConnection;
  /** Reads and enables pagination through a set of `Dokumenty`. */
  dokumentiesByDKdo: DokumentiesConnection;
  /** Reads and enables pagination through a set of `GalerieFoto`. */
  galerieFotosByGfKdo: GalerieFotosConnection;
  /** Reads and enables pagination through a set of `Nabidka`. */
  nabidkasByNTrener: NabidkasConnection;
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  /** Reads and enables pagination through a set of `Pary`. */
  pariesByPIdPartner: PariesConnection;
  /** Reads and enables pagination through a set of `ParyNavrh`. */
  paryNavrhsByPnNavrhl: ParyNavrhsConnection;
  /** Reads and enables pagination through a set of `ParyNavrh`. */
  paryNavrhsByPnPartner: ParyNavrhsConnection;
  /** Reads and enables pagination through a set of `ParyNavrh`. */
  paryNavrhsByPnPartnerka: ParyNavrhsConnection;
  /** Reads a single `Permission` that is related to this `User`. */
  permissionByUGroup?: Maybe<Permission>;
  /** Reads and enables pagination through a set of `PlatbyItem`. */
  platbyItemsByPiIdUser: PlatbyItemsConnection;
  /** Reads and enables pagination through a set of `Rozpi`. */
  rozpisByRTrener: RozpisConnection;
  /** Reads and enables pagination through a set of `Session`. */
  sessionsBySsUser: SessionsConnection;
  /** Reads a single `Skupiny` that is related to this `User`. */
  skupinyByUSkupina?: Maybe<Skupiny>;
  uBan: Scalars['Boolean'];
  uCity: Scalars['String'];
  uConfirmed: Scalars['Boolean'];
  uConscriptionNumber: Scalars['String'];
  uCreatedAt: Scalars['Datetime'];
  uDancer: Scalars['Boolean'];
  uDistrict: Scalars['String'];
  uEmail: Scalars['String'];
  uGdprSignedAt?: Maybe<Scalars['Datetime']>;
  uGroup: Scalars['BigInt'];
  uId: Scalars['BigInt'];
  uJmeno: Scalars['String'];
  uLevel: Scalars['Int'];
  uLock: Scalars['Boolean'];
  uLogin: Scalars['String'];
  uMemberSince?: Maybe<Scalars['Datetime']>;
  uMemberUntil?: Maybe<Scalars['Datetime']>;
  uNarozeni: Scalars['Date'];
  uNationality: Scalars['String'];
  uOrientationNumber: Scalars['String'];
  uPass: Scalars['String'];
  uPohlavi: Scalars['String'];
  uPostalCode: Scalars['String'];
  uPoznamky: Scalars['String'];
  uPrijmeni: Scalars['String'];
  uRodneCislo?: Maybe<Scalars['String']>;
  uSkupina: Scalars['BigInt'];
  uStreet: Scalars['String'];
  uSystem: Scalars['Boolean'];
  uTeacher: Scalars['Boolean'];
  uTelefon: Scalars['String'];
  uTimestamp?: Maybe<Scalars['Datetime']>;
  /** Reads and enables pagination through a set of `Upozorneni`. */
  upozornenisByUpKdo: UpozornenisConnection;
};


export type UserAkceItemsByAiUserArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AkceItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AkceItemsOrderBy>>;
};


export type UserAktualitiesByAtKdoArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<AktualityCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<AktualitiesOrderBy>>;
};


export type UserDokumentiesByDKdoArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<DokumentyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<DokumentiesOrderBy>>;
};


export type UserGalerieFotosByGfKdoArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<GalerieFotoCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<GalerieFotosOrderBy>>;
};


export type UserNabidkasByNTrenerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<NabidkaCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<NabidkasOrderBy>>;
};


export type UserPariesByPIdPartnerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PariesOrderBy>>;
};


export type UserParyNavrhsByPnNavrhlArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyNavrhCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};


export type UserParyNavrhsByPnPartnerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyNavrhCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};


export type UserParyNavrhsByPnPartnerkaArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<ParyNavrhCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<ParyNavrhsOrderBy>>;
};


export type UserPlatbyItemsByPiIdUserArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<PlatbyItemCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<PlatbyItemsOrderBy>>;
};


export type UserRozpisByRTrenerArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<RozpiCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<RozpisOrderBy>>;
};


export type UserSessionsBySsUserArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<SessionCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<SessionsOrderBy>>;
};


export type UserUpozornenisByUpKdoArgs = {
  after?: InputMaybe<Scalars['Cursor']>;
  before?: InputMaybe<Scalars['Cursor']>;
  condition?: InputMaybe<UpozorneniCondition>;
  first?: InputMaybe<Scalars['Int']>;
  last?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  orderBy?: InputMaybe<Array<UpozornenisOrderBy>>;
};

/** A condition to be used against `User` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type UserCondition = {
  /** Checks for equality with the object’s `uBan` field. */
  uBan?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uCity` field. */
  uCity?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uConfirmed` field. */
  uConfirmed?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uConscriptionNumber` field. */
  uConscriptionNumber?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uCreatedAt` field. */
  uCreatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `uDancer` field. */
  uDancer?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uDistrict` field. */
  uDistrict?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uEmail` field. */
  uEmail?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uGdprSignedAt` field. */
  uGdprSignedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `uGroup` field. */
  uGroup?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `uId` field. */
  uId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `uJmeno` field. */
  uJmeno?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uLevel` field. */
  uLevel?: InputMaybe<Scalars['Int']>;
  /** Checks for equality with the object’s `uLock` field. */
  uLock?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uLogin` field. */
  uLogin?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uMemberSince` field. */
  uMemberSince?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `uMemberUntil` field. */
  uMemberUntil?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `uNarozeni` field. */
  uNarozeni?: InputMaybe<Scalars['Date']>;
  /** Checks for equality with the object’s `uNationality` field. */
  uNationality?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uOrientationNumber` field. */
  uOrientationNumber?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uPass` field. */
  uPass?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uPohlavi` field. */
  uPohlavi?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uPostalCode` field. */
  uPostalCode?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uPoznamky` field. */
  uPoznamky?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uPrijmeni` field. */
  uPrijmeni?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uRodneCislo` field. */
  uRodneCislo?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uSkupina` field. */
  uSkupina?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `uStreet` field. */
  uStreet?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uSystem` field. */
  uSystem?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uTeacher` field. */
  uTeacher?: InputMaybe<Scalars['Boolean']>;
  /** Checks for equality with the object’s `uTelefon` field. */
  uTelefon?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `uTimestamp` field. */
  uTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** An input for mutations affecting `User` */
export type UserInput = {
  uBan?: InputMaybe<Scalars['Boolean']>;
  uCity: Scalars['String'];
  uConfirmed?: InputMaybe<Scalars['Boolean']>;
  uConscriptionNumber?: InputMaybe<Scalars['String']>;
  uCreatedAt?: InputMaybe<Scalars['Datetime']>;
  uDancer?: InputMaybe<Scalars['Boolean']>;
  uDistrict?: InputMaybe<Scalars['String']>;
  uEmail: Scalars['String'];
  uGdprSignedAt?: InputMaybe<Scalars['Datetime']>;
  uGroup: Scalars['BigInt'];
  uId?: InputMaybe<Scalars['BigInt']>;
  uJmeno: Scalars['String'];
  uLevel?: InputMaybe<Scalars['Int']>;
  uLock?: InputMaybe<Scalars['Boolean']>;
  uLogin: Scalars['String'];
  uMemberSince?: InputMaybe<Scalars['Datetime']>;
  uMemberUntil?: InputMaybe<Scalars['Datetime']>;
  uNarozeni: Scalars['Date'];
  uNationality: Scalars['String'];
  uOrientationNumber?: InputMaybe<Scalars['String']>;
  uPass: Scalars['String'];
  uPohlavi: Scalars['String'];
  uPostalCode: Scalars['String'];
  uPoznamky?: InputMaybe<Scalars['String']>;
  uPrijmeni: Scalars['String'];
  uRodneCislo?: InputMaybe<Scalars['String']>;
  uSkupina?: InputMaybe<Scalars['BigInt']>;
  uStreet: Scalars['String'];
  uSystem?: InputMaybe<Scalars['Boolean']>;
  uTeacher?: InputMaybe<Scalars['Boolean']>;
  uTelefon: Scalars['String'];
  uTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** Represents an update to a `User`. Fields that are set will be updated. */
export type UserPatch = {
  uBan?: InputMaybe<Scalars['Boolean']>;
  uCity?: InputMaybe<Scalars['String']>;
  uConfirmed?: InputMaybe<Scalars['Boolean']>;
  uConscriptionNumber?: InputMaybe<Scalars['String']>;
  uCreatedAt?: InputMaybe<Scalars['Datetime']>;
  uDancer?: InputMaybe<Scalars['Boolean']>;
  uDistrict?: InputMaybe<Scalars['String']>;
  uEmail?: InputMaybe<Scalars['String']>;
  uGdprSignedAt?: InputMaybe<Scalars['Datetime']>;
  uGroup?: InputMaybe<Scalars['BigInt']>;
  uId?: InputMaybe<Scalars['BigInt']>;
  uJmeno?: InputMaybe<Scalars['String']>;
  uLevel?: InputMaybe<Scalars['Int']>;
  uLock?: InputMaybe<Scalars['Boolean']>;
  uLogin?: InputMaybe<Scalars['String']>;
  uMemberSince?: InputMaybe<Scalars['Datetime']>;
  uMemberUntil?: InputMaybe<Scalars['Datetime']>;
  uNarozeni?: InputMaybe<Scalars['Date']>;
  uNationality?: InputMaybe<Scalars['String']>;
  uOrientationNumber?: InputMaybe<Scalars['String']>;
  uPass?: InputMaybe<Scalars['String']>;
  uPohlavi?: InputMaybe<Scalars['String']>;
  uPostalCode?: InputMaybe<Scalars['String']>;
  uPoznamky?: InputMaybe<Scalars['String']>;
  uPrijmeni?: InputMaybe<Scalars['String']>;
  uRodneCislo?: InputMaybe<Scalars['String']>;
  uSkupina?: InputMaybe<Scalars['BigInt']>;
  uStreet?: InputMaybe<Scalars['String']>;
  uSystem?: InputMaybe<Scalars['Boolean']>;
  uTeacher?: InputMaybe<Scalars['Boolean']>;
  uTelefon?: InputMaybe<Scalars['String']>;
  uTimestamp?: InputMaybe<Scalars['Datetime']>;
};

/** A connection to a list of `User` values. */
export type UsersConnection = {
  __typename?: 'UsersConnection';
  /** A list of edges which contains the `User` and cursor to aid in pagination. */
  edges: Array<UsersEdge>;
  /** A list of `User` objects. */
  nodes: Array<User>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `User` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `User` edge in the connection. */
export type UsersEdge = {
  __typename?: 'UsersEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `User` at the end of the edge. */
  node: User;
};

/** Methods to use when ordering `User`. */
export enum UsersOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  UBanAsc = 'U_BAN_ASC',
  UBanDesc = 'U_BAN_DESC',
  UCityAsc = 'U_CITY_ASC',
  UCityDesc = 'U_CITY_DESC',
  UConfirmedAsc = 'U_CONFIRMED_ASC',
  UConfirmedDesc = 'U_CONFIRMED_DESC',
  UConscriptionNumberAsc = 'U_CONSCRIPTION_NUMBER_ASC',
  UConscriptionNumberDesc = 'U_CONSCRIPTION_NUMBER_DESC',
  UCreatedAtAsc = 'U_CREATED_AT_ASC',
  UCreatedAtDesc = 'U_CREATED_AT_DESC',
  UDancerAsc = 'U_DANCER_ASC',
  UDancerDesc = 'U_DANCER_DESC',
  UDistrictAsc = 'U_DISTRICT_ASC',
  UDistrictDesc = 'U_DISTRICT_DESC',
  UEmailAsc = 'U_EMAIL_ASC',
  UEmailDesc = 'U_EMAIL_DESC',
  UGdprSignedAtAsc = 'U_GDPR_SIGNED_AT_ASC',
  UGdprSignedAtDesc = 'U_GDPR_SIGNED_AT_DESC',
  UGroupAsc = 'U_GROUP_ASC',
  UGroupDesc = 'U_GROUP_DESC',
  UIdAsc = 'U_ID_ASC',
  UIdDesc = 'U_ID_DESC',
  UJmenoAsc = 'U_JMENO_ASC',
  UJmenoDesc = 'U_JMENO_DESC',
  ULevelAsc = 'U_LEVEL_ASC',
  ULevelDesc = 'U_LEVEL_DESC',
  ULockAsc = 'U_LOCK_ASC',
  ULockDesc = 'U_LOCK_DESC',
  ULoginAsc = 'U_LOGIN_ASC',
  ULoginDesc = 'U_LOGIN_DESC',
  UMemberSinceAsc = 'U_MEMBER_SINCE_ASC',
  UMemberSinceDesc = 'U_MEMBER_SINCE_DESC',
  UMemberUntilAsc = 'U_MEMBER_UNTIL_ASC',
  UMemberUntilDesc = 'U_MEMBER_UNTIL_DESC',
  UNarozeniAsc = 'U_NAROZENI_ASC',
  UNarozeniDesc = 'U_NAROZENI_DESC',
  UNationalityAsc = 'U_NATIONALITY_ASC',
  UNationalityDesc = 'U_NATIONALITY_DESC',
  UOrientationNumberAsc = 'U_ORIENTATION_NUMBER_ASC',
  UOrientationNumberDesc = 'U_ORIENTATION_NUMBER_DESC',
  UPassAsc = 'U_PASS_ASC',
  UPassDesc = 'U_PASS_DESC',
  UPohlaviAsc = 'U_POHLAVI_ASC',
  UPohlaviDesc = 'U_POHLAVI_DESC',
  UPostalCodeAsc = 'U_POSTAL_CODE_ASC',
  UPostalCodeDesc = 'U_POSTAL_CODE_DESC',
  UPoznamkyAsc = 'U_POZNAMKY_ASC',
  UPoznamkyDesc = 'U_POZNAMKY_DESC',
  UPrijmeniAsc = 'U_PRIJMENI_ASC',
  UPrijmeniDesc = 'U_PRIJMENI_DESC',
  URodneCisloAsc = 'U_RODNE_CISLO_ASC',
  URodneCisloDesc = 'U_RODNE_CISLO_DESC',
  USkupinaAsc = 'U_SKUPINA_ASC',
  USkupinaDesc = 'U_SKUPINA_DESC',
  UStreetAsc = 'U_STREET_ASC',
  UStreetDesc = 'U_STREET_DESC',
  USystemAsc = 'U_SYSTEM_ASC',
  USystemDesc = 'U_SYSTEM_DESC',
  UTeacherAsc = 'U_TEACHER_ASC',
  UTeacherDesc = 'U_TEACHER_DESC',
  UTelefonAsc = 'U_TELEFON_ASC',
  UTelefonDesc = 'U_TELEFON_DESC',
  UTimestampAsc = 'U_TIMESTAMP_ASC',
  UTimestampDesc = 'U_TIMESTAMP_DESC'
}

/** A connection to a list of `UsersSkupiny` values. */
export type UsersSkupiniesConnection = {
  __typename?: 'UsersSkupiniesConnection';
  /** A list of edges which contains the `UsersSkupiny` and cursor to aid in pagination. */
  edges: Array<UsersSkupiniesEdge>;
  /** A list of `UsersSkupiny` objects. */
  nodes: Array<UsersSkupiny>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `UsersSkupiny` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `UsersSkupiny` edge in the connection. */
export type UsersSkupiniesEdge = {
  __typename?: 'UsersSkupiniesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `UsersSkupiny` at the end of the edge. */
  node: UsersSkupiny;
};

/** Methods to use when ordering `UsersSkupiny`. */
export enum UsersSkupiniesOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  UsColorAsc = 'US_COLOR_ASC',
  UsColorDesc = 'US_COLOR_DESC',
  UsIdAsc = 'US_ID_ASC',
  UsIdDesc = 'US_ID_DESC',
  UsPlatbaCtvrtrokAsc = 'US_PLATBA_CTVRTROK_ASC',
  UsPlatbaCtvrtrokDesc = 'US_PLATBA_CTVRTROK_DESC',
  UsPlatbaMesicAsc = 'US_PLATBA_MESIC_ASC',
  UsPlatbaMesicDesc = 'US_PLATBA_MESIC_DESC',
  UsPlatbaPulrokAsc = 'US_PLATBA_PULROK_ASC',
  UsPlatbaPulrokDesc = 'US_PLATBA_PULROK_DESC',
  UsPopisAsc = 'US_POPIS_ASC',
  UsPopisDesc = 'US_POPIS_DESC'
}

export type UsersSkupiny = Node & {
  __typename?: 'UsersSkupiny';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  usColor: Scalars['String'];
  usId: Scalars['BigInt'];
  usPlatbaCtvrtrok: Scalars['BigInt'];
  usPlatbaMesic: Scalars['BigInt'];
  usPlatbaPulrok: Scalars['BigInt'];
  usPopis: Scalars['String'];
};

/**
 * A condition to be used against `UsersSkupiny` object types. All fields are
 * tested for equality and combined with a logical ‘and.’
 */
export type UsersSkupinyCondition = {
  /** Checks for equality with the object’s `usColor` field. */
  usColor?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `usId` field. */
  usId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `usPlatbaCtvrtrok` field. */
  usPlatbaCtvrtrok?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `usPlatbaMesic` field. */
  usPlatbaMesic?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `usPlatbaPulrok` field. */
  usPlatbaPulrok?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `usPopis` field. */
  usPopis?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `UsersSkupiny` */
export type UsersSkupinyInput = {
  usColor?: InputMaybe<Scalars['String']>;
  usId?: InputMaybe<Scalars['BigInt']>;
  usPlatbaCtvrtrok?: InputMaybe<Scalars['BigInt']>;
  usPlatbaMesic?: InputMaybe<Scalars['BigInt']>;
  usPlatbaPulrok?: InputMaybe<Scalars['BigInt']>;
  usPopis: Scalars['String'];
};

/** Represents an update to a `UsersSkupiny`. Fields that are set will be updated. */
export type UsersSkupinyPatch = {
  usColor?: InputMaybe<Scalars['String']>;
  usId?: InputMaybe<Scalars['BigInt']>;
  usPlatbaCtvrtrok?: InputMaybe<Scalars['BigInt']>;
  usPlatbaMesic?: InputMaybe<Scalars['BigInt']>;
  usPlatbaPulrok?: InputMaybe<Scalars['BigInt']>;
  usPopis?: InputMaybe<Scalars['String']>;
};

export type Video = Node & {
  __typename?: 'Video';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  vAuthor: Scalars['String'];
  vCreatedAt: Scalars['Datetime'];
  vDescription: Scalars['String'];
  vId: Scalars['BigInt'];
  vPlaylist?: Maybe<Scalars['String']>;
  vTitle: Scalars['String'];
  vUpdatedAt: Scalars['Datetime'];
  vUri: Scalars['String'];
};

/** A condition to be used against `Video` object types. All fields are tested for equality and combined with a logical ‘and.’ */
export type VideoCondition = {
  /** Checks for equality with the object’s `vAuthor` field. */
  vAuthor?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vCreatedAt` field. */
  vCreatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vDescription` field. */
  vDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vId` field. */
  vId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `vPlaylist` field. */
  vPlaylist?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vTitle` field. */
  vTitle?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vUpdatedAt` field. */
  vUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vUri` field. */
  vUri?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `Video` */
export type VideoInput = {
  vAuthor: Scalars['String'];
  vCreatedAt: Scalars['Datetime'];
  vDescription: Scalars['String'];
  vId?: InputMaybe<Scalars['BigInt']>;
  vPlaylist?: InputMaybe<Scalars['String']>;
  vTitle: Scalars['String'];
  vUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  vUri: Scalars['String'];
};

export type VideoList = Node & {
  __typename?: 'VideoList';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  vlCount: Scalars['BigInt'];
  vlCreatedAt: Scalars['Datetime'];
  vlDescription: Scalars['String'];
  vlId: Scalars['BigInt'];
  vlLastChecked?: Maybe<Scalars['Datetime']>;
  vlTitle: Scalars['String'];
  vlUrl: Scalars['String'];
};

/**
 * A condition to be used against `VideoList` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type VideoListCondition = {
  /** Checks for equality with the object’s `vlCount` field. */
  vlCount?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `vlCreatedAt` field. */
  vlCreatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vlDescription` field. */
  vlDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vlId` field. */
  vlId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `vlLastChecked` field. */
  vlLastChecked?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vlTitle` field. */
  vlTitle?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vlUrl` field. */
  vlUrl?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `VideoList` */
export type VideoListInput = {
  vlCount: Scalars['BigInt'];
  vlCreatedAt: Scalars['Datetime'];
  vlDescription: Scalars['String'];
  vlId?: InputMaybe<Scalars['BigInt']>;
  vlLastChecked?: InputMaybe<Scalars['Datetime']>;
  vlTitle: Scalars['String'];
  vlUrl: Scalars['String'];
};

/** Represents an update to a `VideoList`. Fields that are set will be updated. */
export type VideoListPatch = {
  vlCount?: InputMaybe<Scalars['BigInt']>;
  vlCreatedAt?: InputMaybe<Scalars['Datetime']>;
  vlDescription?: InputMaybe<Scalars['String']>;
  vlId?: InputMaybe<Scalars['BigInt']>;
  vlLastChecked?: InputMaybe<Scalars['Datetime']>;
  vlTitle?: InputMaybe<Scalars['String']>;
  vlUrl?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `VideoList` values. */
export type VideoListsConnection = {
  __typename?: 'VideoListsConnection';
  /** A list of edges which contains the `VideoList` and cursor to aid in pagination. */
  edges: Array<VideoListsEdge>;
  /** A list of `VideoList` objects. */
  nodes: Array<VideoList>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `VideoList` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `VideoList` edge in the connection. */
export type VideoListsEdge = {
  __typename?: 'VideoListsEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `VideoList` at the end of the edge. */
  node: VideoList;
};

/** Methods to use when ordering `VideoList`. */
export enum VideoListsOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  VlCountAsc = 'VL_COUNT_ASC',
  VlCountDesc = 'VL_COUNT_DESC',
  VlCreatedAtAsc = 'VL_CREATED_AT_ASC',
  VlCreatedAtDesc = 'VL_CREATED_AT_DESC',
  VlDescriptionAsc = 'VL_DESCRIPTION_ASC',
  VlDescriptionDesc = 'VL_DESCRIPTION_DESC',
  VlIdAsc = 'VL_ID_ASC',
  VlIdDesc = 'VL_ID_DESC',
  VlLastCheckedAsc = 'VL_LAST_CHECKED_ASC',
  VlLastCheckedDesc = 'VL_LAST_CHECKED_DESC',
  VlTitleAsc = 'VL_TITLE_ASC',
  VlTitleDesc = 'VL_TITLE_DESC',
  VlUrlAsc = 'VL_URL_ASC',
  VlUrlDesc = 'VL_URL_DESC'
}

/** Represents an update to a `Video`. Fields that are set will be updated. */
export type VideoPatch = {
  vAuthor?: InputMaybe<Scalars['String']>;
  vCreatedAt?: InputMaybe<Scalars['Datetime']>;
  vDescription?: InputMaybe<Scalars['String']>;
  vId?: InputMaybe<Scalars['BigInt']>;
  vPlaylist?: InputMaybe<Scalars['String']>;
  vTitle?: InputMaybe<Scalars['String']>;
  vUpdatedAt?: InputMaybe<Scalars['Datetime']>;
  vUri?: InputMaybe<Scalars['String']>;
};

export type VideoSource = Node & {
  __typename?: 'VideoSource';
  /** A globally unique identifier. Can be used in various places throughout the system to identify this single value. */
  nodeId: Scalars['ID'];
  vsCreatedAt: Scalars['Datetime'];
  vsDescription?: Maybe<Scalars['String']>;
  vsId: Scalars['BigInt'];
  vsLastChecked?: Maybe<Scalars['Datetime']>;
  vsTitle?: Maybe<Scalars['String']>;
  vsUrl: Scalars['String'];
};

/**
 * A condition to be used against `VideoSource` object types. All fields are tested
 * for equality and combined with a logical ‘and.’
 */
export type VideoSourceCondition = {
  /** Checks for equality with the object’s `vsCreatedAt` field. */
  vsCreatedAt?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vsDescription` field. */
  vsDescription?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vsId` field. */
  vsId?: InputMaybe<Scalars['BigInt']>;
  /** Checks for equality with the object’s `vsLastChecked` field. */
  vsLastChecked?: InputMaybe<Scalars['Datetime']>;
  /** Checks for equality with the object’s `vsTitle` field. */
  vsTitle?: InputMaybe<Scalars['String']>;
  /** Checks for equality with the object’s `vsUrl` field. */
  vsUrl?: InputMaybe<Scalars['String']>;
};

/** An input for mutations affecting `VideoSource` */
export type VideoSourceInput = {
  vsCreatedAt?: InputMaybe<Scalars['Datetime']>;
  vsDescription?: InputMaybe<Scalars['String']>;
  vsId?: InputMaybe<Scalars['BigInt']>;
  vsLastChecked?: InputMaybe<Scalars['Datetime']>;
  vsTitle?: InputMaybe<Scalars['String']>;
  vsUrl: Scalars['String'];
};

/** Represents an update to a `VideoSource`. Fields that are set will be updated. */
export type VideoSourcePatch = {
  vsCreatedAt?: InputMaybe<Scalars['Datetime']>;
  vsDescription?: InputMaybe<Scalars['String']>;
  vsId?: InputMaybe<Scalars['BigInt']>;
  vsLastChecked?: InputMaybe<Scalars['Datetime']>;
  vsTitle?: InputMaybe<Scalars['String']>;
  vsUrl?: InputMaybe<Scalars['String']>;
};

/** A connection to a list of `VideoSource` values. */
export type VideoSourcesConnection = {
  __typename?: 'VideoSourcesConnection';
  /** A list of edges which contains the `VideoSource` and cursor to aid in pagination. */
  edges: Array<VideoSourcesEdge>;
  /** A list of `VideoSource` objects. */
  nodes: Array<VideoSource>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `VideoSource` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `VideoSource` edge in the connection. */
export type VideoSourcesEdge = {
  __typename?: 'VideoSourcesEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `VideoSource` at the end of the edge. */
  node: VideoSource;
};

/** Methods to use when ordering `VideoSource`. */
export enum VideoSourcesOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  VsCreatedAtAsc = 'VS_CREATED_AT_ASC',
  VsCreatedAtDesc = 'VS_CREATED_AT_DESC',
  VsDescriptionAsc = 'VS_DESCRIPTION_ASC',
  VsDescriptionDesc = 'VS_DESCRIPTION_DESC',
  VsIdAsc = 'VS_ID_ASC',
  VsIdDesc = 'VS_ID_DESC',
  VsLastCheckedAsc = 'VS_LAST_CHECKED_ASC',
  VsLastCheckedDesc = 'VS_LAST_CHECKED_DESC',
  VsTitleAsc = 'VS_TITLE_ASC',
  VsTitleDesc = 'VS_TITLE_DESC',
  VsUrlAsc = 'VS_URL_ASC',
  VsUrlDesc = 'VS_URL_DESC'
}

/** A connection to a list of `Video` values. */
export type VideosConnection = {
  __typename?: 'VideosConnection';
  /** A list of edges which contains the `Video` and cursor to aid in pagination. */
  edges: Array<VideosEdge>;
  /** A list of `Video` objects. */
  nodes: Array<Video>;
  /** Information to aid in pagination. */
  pageInfo: PageInfo;
  /** The count of *all* `Video` you could get from the connection. */
  totalCount: Scalars['Int'];
};

/** A `Video` edge in the connection. */
export type VideosEdge = {
  __typename?: 'VideosEdge';
  /** A cursor for use in pagination. */
  cursor?: Maybe<Scalars['Cursor']>;
  /** The `Video` at the end of the edge. */
  node: Video;
};

/** Methods to use when ordering `Video`. */
export enum VideosOrderBy {
  Natural = 'NATURAL',
  PrimaryKeyAsc = 'PRIMARY_KEY_ASC',
  PrimaryKeyDesc = 'PRIMARY_KEY_DESC',
  VAuthorAsc = 'V_AUTHOR_ASC',
  VAuthorDesc = 'V_AUTHOR_DESC',
  VCreatedAtAsc = 'V_CREATED_AT_ASC',
  VCreatedAtDesc = 'V_CREATED_AT_DESC',
  VDescriptionAsc = 'V_DESCRIPTION_ASC',
  VDescriptionDesc = 'V_DESCRIPTION_DESC',
  VIdAsc = 'V_ID_ASC',
  VIdDesc = 'V_ID_DESC',
  VPlaylistAsc = 'V_PLAYLIST_ASC',
  VPlaylistDesc = 'V_PLAYLIST_DESC',
  VTitleAsc = 'V_TITLE_ASC',
  VTitleDesc = 'V_TITLE_DESC',
  VUpdatedAtAsc = 'V_UPDATED_AT_ASC',
  VUpdatedAtDesc = 'V_UPDATED_AT_DESC',
  VUriAsc = 'V_URI_ASC',
  VUriDesc = 'V_URI_DESC'
}

export type UserQueryQueryVariables = Exact<{ [key: string]: never; }>;


export type UserQueryQuery = { __typename?: 'Query', getCurrentUser?: { __typename?: 'User', uId: any, uJmeno: string, uPrijmeni: string, permissionByUGroup?: { __typename?: 'Permission', peAkce: number, peAnkety: number, peAktuality: number, peDescription: string, peDokumenty: number, peGalerie: number, peId: any, peKonzole: number, peInzerce: number, peNabidka: number, peMain: number, peName: string, peNastenka: number, peNovinky: number, pePary: number, pePermissions: number, pePlatby: number, peRozpis: number, peSkupiny: number, peUsers: number } | null | undefined } | null | undefined };

export type UpozorneniListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type UpozorneniListQuery = { __typename?: 'Query', allUpozornenis?: { __typename?: 'UpozornenisConnection', totalCount: number, nodes: Array<{ __typename?: 'Upozorneni', upId: any, upKdo: any, upLock: boolean, upNadpis: string, upText: string, upTimestamp?: any | null | undefined, upTimestampAdd: any, userByUpKdo?: { __typename?: 'User', uId: any, uJmeno: string, uPrijmeni: string } | null | undefined, upozorneniSkupiniesByUpsIdRodic: { __typename?: 'UpozorneniSkupiniesConnection', nodes: Array<{ __typename?: 'UpozorneniSkupiny', skupinyByUpsIdSkupina?: { __typename?: 'Skupiny', sName: string, sDescription: string, sColorText: string, sColorRgb: string } | null | undefined }> } }> } | null | undefined };

export type ArticlesAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ArticlesAdminListQuery = { __typename?: 'Query', allAktualities?: { __typename?: 'AktualitiesConnection', totalCount: number, nodes: Array<{ __typename?: 'Aktuality', atFoto?: any | null | undefined, atFotoMain?: any | null | undefined, atId: any, atJmeno: string, atKdo: any, atPreview: string, atText: string, atTimestampAdd?: any | null | undefined, atTimestamp?: any | null | undefined }> } | null | undefined };

export type AkceListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type AkceListQuery = { __typename?: 'Query', allAkces?: { __typename?: 'AkcesConnection', totalCount: number, nodes: Array<{ __typename?: 'Akce', aDo: any, aId: any, aInfo: string, aDokumenty: string, aJmeno: string, aKapacita: any, aKde: string, aLock: boolean, aOd: any, aTimestamp?: any | null | undefined, aVisible: boolean, akceItemsByAiIdRodic: { __typename?: 'AkceItemsConnection', totalCount: number, nodes: Array<{ __typename?: 'AkceItem', aiId: any, userByAiUser?: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uId: any } | null | undefined }> } }> } | null | undefined };

export type SetAkceVisibleMutationVariables = Exact<{
  id: Scalars['BigInt'];
  visible: Scalars['Boolean'];
}>;


export type SetAkceVisibleMutation = { __typename?: 'Mutation', updateAkceByAId?: { __typename?: 'UpdateAkcePayload', akce?: { __typename?: 'Akce', aId: any } | null | undefined } | null | undefined };

export type GalleryDirListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type GalleryDirListQuery = { __typename?: 'Query', allGalerieDirs?: { __typename?: 'GalerieDirsConnection', totalCount: number, nodes: Array<{ __typename?: 'GalerieDir', gdHidden: boolean, gdId: any, gdIdRodic: any, gdLevel: number, gdName: string, gdPath: string }> } | null | undefined };

export type SetGalerieDirVisibleMutationVariables = Exact<{
  id: Scalars['BigInt'];
  visible: Scalars['Boolean'];
}>;


export type SetGalerieDirVisibleMutation = { __typename?: 'Mutation', updateGalerieDirByGdId?: { __typename?: 'UpdateGalerieDirPayload', galerieDir?: { __typename?: 'GalerieDir', gdId: any } | null | undefined } | null | undefined };

export type ReservationAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ReservationAdminListQuery = { __typename?: 'Query', allNabidkas?: { __typename?: 'NabidkasConnection', totalCount: number, nodes: Array<{ __typename?: 'Nabidka', nDo: any, nId: any, nLock: boolean, nMaxPocetHod: any, nOd: any, nPocetHod: number, nTimestamp?: any | null | undefined, nTrener: any, nVisible: boolean, nabidkaItemsByNiIdRodic: { __typename?: 'NabidkaItemsConnection', nodes: Array<{ __typename?: 'NabidkaItem', niPocetHod: number, niPartner: any, niLock: boolean, paryByNiPartner?: { __typename?: 'Pary', userByPIdPartner?: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uId: any } | null | undefined } | null | undefined }> }, userByNTrener?: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uId: any } | null | undefined }> } | null | undefined };

export type SetNabidkaVisibleMutationVariables = Exact<{
  id: Scalars['BigInt'];
  visible: Scalars['Boolean'];
}>;


export type SetNabidkaVisibleMutation = { __typename?: 'Mutation', updateNabidkaByNId?: { __typename?: 'UpdateNabidkaPayload', nabidka?: { __typename?: 'Nabidka', nId: any } | null | undefined } | null | undefined };

export type NabidkaListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type NabidkaListQuery = { __typename?: 'Query', allNabidkas?: { __typename?: 'NabidkasConnection', totalCount: number, nodes: Array<{ __typename?: 'Nabidka', nDo: any, nId: any, nLock: boolean, nMaxPocetHod: any, nOd: any, nPocetHod: number, nTimestamp?: any | null | undefined, nTrener: any, nVisible: boolean, nabidkaItemsByNiIdRodic: { __typename?: 'NabidkaItemsConnection', nodes: Array<{ __typename?: 'NabidkaItem', niPocetHod: number, niPartner: any, niLock: boolean, paryByNiPartner?: { __typename?: 'Pary', userByPIdPartner?: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uId: any } | null | undefined } | null | undefined }> }, userByNTrener?: { __typename?: 'User', uJmeno: string, uPrijmeni: string, uId: any } | null | undefined }> } | null | undefined };

export type ScheduleAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ScheduleAdminListQuery = { __typename?: 'Query', allRozpis?: { __typename?: 'RozpisConnection', totalCount: number, nodes: Array<{ __typename?: 'Rozpi', rDatum: any, rId: any, rKde: string, rLock: boolean, rTimestamp?: any | null | undefined, rTrener: any, rVisible: boolean, userByRTrener?: { __typename?: 'User', uId: any, uJmeno: string, uPrijmeni: string } | null | undefined, rozpisItemsByRiIdRodic: { __typename?: 'RozpisItemsConnection', nodes: Array<{ __typename?: 'RozpisItem', riDo: any, riOd: any, riId: any, riPartner?: any | null | undefined }> } }> } | null | undefined };

export type SetRozpisVisibleMutationVariables = Exact<{
  id: Scalars['BigInt'];
  visible: Scalars['Boolean'];
}>;


export type SetRozpisVisibleMutation = { __typename?: 'Mutation', updateRozpiByRId?: { __typename?: 'UpdateRozpiPayload', rozpi?: { __typename?: 'Rozpi', rId: any } | null | undefined } | null | undefined };

export type GetPageQueryVariables = Exact<{
  url: Scalars['String'];
}>;


export type GetPageQuery = { __typename?: 'Query', pageByUrl?: { __typename?: 'Page', content: any } | null | undefined };

export type GetPhotoQueryVariables = Exact<{
  id: Scalars['BigInt'];
}>;


export type GetPhotoQuery = { __typename?: 'Query', galerieFotoByGfId?: { __typename?: 'GalerieFoto', gfId: any, gfIdRodic: any, gfKdo: any, gfName: string, gfPath: string, gfTimestamp?: any | null | undefined, galerieDirByGfIdRodic?: { __typename?: 'GalerieDir', gdId: any } | null | undefined } | null | undefined };

export type SignInMutationVariables = Exact<{
  login: Scalars['String'];
  passwd: Scalars['String'];
}>;


export type SignInMutation = { __typename?: 'Mutation', login?: { __typename?: 'LoginPayload', query?: { __typename?: 'Query', currentUserId?: any | null | undefined } | null | undefined } | null | undefined };

export type GetMenuQueryVariables = Exact<{ [key: string]: never; }>;


export type GetMenuQuery = { __typename?: 'Query', parameterByPaName?: { __typename?: 'Parameter', paValue: string } | null | undefined };


export const UserQueryDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"UserQuery"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"getCurrentUser"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"permissionByUGroup"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"peAkce"}},{"kind":"Field","name":{"kind":"Name","value":"peAnkety"}},{"kind":"Field","name":{"kind":"Name","value":"peAktuality"}},{"kind":"Field","name":{"kind":"Name","value":"peDescription"}},{"kind":"Field","name":{"kind":"Name","value":"peDokumenty"}},{"kind":"Field","name":{"kind":"Name","value":"peGalerie"}},{"kind":"Field","name":{"kind":"Name","value":"peId"}},{"kind":"Field","name":{"kind":"Name","value":"peKonzole"}},{"kind":"Field","name":{"kind":"Name","value":"peInzerce"}},{"kind":"Field","name":{"kind":"Name","value":"peNabidka"}},{"kind":"Field","name":{"kind":"Name","value":"peMain"}},{"kind":"Field","name":{"kind":"Name","value":"peName"}},{"kind":"Field","name":{"kind":"Name","value":"peNastenka"}},{"kind":"Field","name":{"kind":"Name","value":"peNovinky"}},{"kind":"Field","name":{"kind":"Name","value":"pePary"}},{"kind":"Field","name":{"kind":"Name","value":"pePermissions"}},{"kind":"Field","name":{"kind":"Name","value":"pePlatby"}},{"kind":"Field","name":{"kind":"Name","value":"peRozpis"}},{"kind":"Field","name":{"kind":"Name","value":"peSkupiny"}},{"kind":"Field","name":{"kind":"Name","value":"peUsers"}}]}},{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}}]}}]} as unknown as DocumentNode<UserQueryQuery, UserQueryQueryVariables>;
export const UpozorneniListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"UpozorneniList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allUpozornenis"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"UP_TIMESTAMP_ADD_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"upId"}},{"kind":"Field","name":{"kind":"Name","value":"upKdo"}},{"kind":"Field","name":{"kind":"Name","value":"upLock"}},{"kind":"Field","name":{"kind":"Name","value":"upNadpis"}},{"kind":"Field","name":{"kind":"Name","value":"upText"}},{"kind":"Field","name":{"kind":"Name","value":"upTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"upTimestampAdd"}},{"kind":"Field","name":{"kind":"Name","value":"userByUpKdo"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"upozorneniSkupiniesByUpsIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"skupinyByUpsIdSkupina"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"sName"}},{"kind":"Field","name":{"kind":"Name","value":"sDescription"}},{"kind":"Field","name":{"kind":"Name","value":"sColorText"}},{"kind":"Field","name":{"kind":"Name","value":"sColorRgb"}}]}}]}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<UpozorneniListQuery, UpozorneniListQueryVariables>;
export const ArticlesAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ArticlesAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allAktualities"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"AT_TIMESTAMP_ADD_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"atFoto"}},{"kind":"Field","name":{"kind":"Name","value":"atFotoMain"}},{"kind":"Field","name":{"kind":"Name","value":"atId"}},{"kind":"Field","name":{"kind":"Name","value":"atJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"atKdo"}},{"kind":"Field","name":{"kind":"Name","value":"atPreview"}},{"kind":"Field","name":{"kind":"Name","value":"atText"}},{"kind":"Field","name":{"kind":"Name","value":"atTimestampAdd"}},{"kind":"Field","name":{"kind":"Name","value":"atTimestamp"}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<ArticlesAdminListQuery, ArticlesAdminListQueryVariables>;
export const AkceListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"AkceList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allAkces"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"A_OD_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aDo"}},{"kind":"Field","name":{"kind":"Name","value":"aId"}},{"kind":"Field","name":{"kind":"Name","value":"aInfo"}},{"kind":"Field","name":{"kind":"Name","value":"aDokumenty"}},{"kind":"Field","name":{"kind":"Name","value":"aJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"aKapacita"}},{"kind":"Field","name":{"kind":"Name","value":"aKde"}},{"kind":"Field","name":{"kind":"Name","value":"aLock"}},{"kind":"Field","name":{"kind":"Name","value":"aOd"}},{"kind":"Field","name":{"kind":"Name","value":"aTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"aVisible"}},{"kind":"Field","name":{"kind":"Name","value":"akceItemsByAiIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aiId"}},{"kind":"Field","name":{"kind":"Name","value":"userByAiUser"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uId"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<AkceListQuery, AkceListQueryVariables>;
export const SetAkceVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setAkceVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateAkceByAId"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"aId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"akcePatch"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"aVisible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"akce"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aId"}}]}}]}}]}}]} as unknown as DocumentNode<SetAkceVisibleMutation, SetAkceVisibleMutationVariables>;
export const GalleryDirListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GalleryDirList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allGalerieDirs"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"GD_NAME_ASC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gdHidden"}},{"kind":"Field","name":{"kind":"Name","value":"gdId"}},{"kind":"Field","name":{"kind":"Name","value":"gdIdRodic"}},{"kind":"Field","name":{"kind":"Name","value":"gdLevel"}},{"kind":"Field","name":{"kind":"Name","value":"gdName"}},{"kind":"Field","name":{"kind":"Name","value":"gdPath"}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<GalleryDirListQuery, GalleryDirListQueryVariables>;
export const SetGalerieDirVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setGalerieDirVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateGalerieDirByGdId"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"gdId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"galerieDirPatch"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"gdHidden"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"galerieDir"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gdId"}}]}}]}}]}}]} as unknown as DocumentNode<SetGalerieDirVisibleMutation, SetGalerieDirVisibleMutationVariables>;
export const ReservationAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ReservationAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allNabidkas"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"N_OD_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nDo"}},{"kind":"Field","name":{"kind":"Name","value":"nId"}},{"kind":"Field","name":{"kind":"Name","value":"nLock"}},{"kind":"Field","name":{"kind":"Name","value":"nMaxPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"nOd"}},{"kind":"Field","name":{"kind":"Name","value":"nPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"nTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"nTrener"}},{"kind":"Field","name":{"kind":"Name","value":"nVisible"}},{"kind":"Field","name":{"kind":"Name","value":"nabidkaItemsByNiIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"niPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"niPartner"}},{"kind":"Field","name":{"kind":"Name","value":"niLock"}},{"kind":"Field","name":{"kind":"Name","value":"paryByNiPartner"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"userByPIdPartner"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uId"}}]}}]}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"userByNTrener"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uId"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<ReservationAdminListQuery, ReservationAdminListQueryVariables>;
export const SetNabidkaVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setNabidkaVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateNabidkaByNId"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"nabidkaPatch"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"nVisible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}},{"kind":"ObjectField","name":{"kind":"Name","value":"nId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nabidka"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nId"}}]}}]}}]}}]} as unknown as DocumentNode<SetNabidkaVisibleMutation, SetNabidkaVisibleMutationVariables>;
export const NabidkaListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"NabidkaList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allNabidkas"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"N_OD_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nDo"}},{"kind":"Field","name":{"kind":"Name","value":"nId"}},{"kind":"Field","name":{"kind":"Name","value":"nLock"}},{"kind":"Field","name":{"kind":"Name","value":"nMaxPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"nOd"}},{"kind":"Field","name":{"kind":"Name","value":"nPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"nTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"nTrener"}},{"kind":"Field","name":{"kind":"Name","value":"nVisible"}},{"kind":"Field","name":{"kind":"Name","value":"nabidkaItemsByNiIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"niPocetHod"}},{"kind":"Field","name":{"kind":"Name","value":"niPartner"}},{"kind":"Field","name":{"kind":"Name","value":"niLock"}},{"kind":"Field","name":{"kind":"Name","value":"paryByNiPartner"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"userByPIdPartner"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uId"}}]}}]}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"userByNTrener"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"uId"}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<NabidkaListQuery, NabidkaListQueryVariables>;
export const ScheduleAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ScheduleAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"allRozpis"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"first"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"orderBy"},"value":{"kind":"EnumValue","value":"R_DATUM_DESC"}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"rDatum"}},{"kind":"Field","name":{"kind":"Name","value":"rId"}},{"kind":"Field","name":{"kind":"Name","value":"rKde"}},{"kind":"Field","name":{"kind":"Name","value":"rLock"}},{"kind":"Field","name":{"kind":"Name","value":"rTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"rTrener"}},{"kind":"Field","name":{"kind":"Name","value":"rVisible"}},{"kind":"Field","name":{"kind":"Name","value":"userByRTrener"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"uId"}},{"kind":"Field","name":{"kind":"Name","value":"uJmeno"}},{"kind":"Field","name":{"kind":"Name","value":"uPrijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"rozpisItemsByRiIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nodes"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"riDo"}},{"kind":"Field","name":{"kind":"Name","value":"riOd"}},{"kind":"Field","name":{"kind":"Name","value":"riId"}},{"kind":"Field","name":{"kind":"Name","value":"riPartner"}}]}}]}}]}},{"kind":"Field","name":{"kind":"Name","value":"totalCount"}}]}}]}}]} as unknown as DocumentNode<ScheduleAdminListQuery, ScheduleAdminListQueryVariables>;
export const SetRozpisVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setRozpisVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"updateRozpiByRId"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"rozpiPatch"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"rVisible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}},{"kind":"ObjectField","name":{"kind":"Name","value":"rId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"rozpi"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"rId"}}]}}]}}]}}]} as unknown as DocumentNode<SetRozpisVisibleMutation, SetRozpisVisibleMutationVariables>;
export const GetPageDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetPage"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"url"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"String"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"pageByUrl"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"url"},"value":{"kind":"Variable","name":{"kind":"Name","value":"url"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"content"}}]}}]}}]} as unknown as DocumentNode<GetPageQuery, GetPageQueryVariables>;
export const GetPhotoDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetPhoto"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"BigInt"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"galerieFotoByGfId"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"gfId"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gfId"}},{"kind":"Field","name":{"kind":"Name","value":"gfIdRodic"}},{"kind":"Field","name":{"kind":"Name","value":"gfKdo"}},{"kind":"Field","name":{"kind":"Name","value":"gfName"}},{"kind":"Field","name":{"kind":"Name","value":"gfPath"}},{"kind":"Field","name":{"kind":"Name","value":"gfTimestamp"}},{"kind":"Field","name":{"kind":"Name","value":"galerieDirByGfIdRodic"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gdId"}}]}}]}}]}}]} as unknown as DocumentNode<GetPhotoQuery, GetPhotoQueryVariables>;
export const SignInDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"SignIn"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"login"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"String"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"passwd"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"String"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"login"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"input"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"login"},"value":{"kind":"Variable","name":{"kind":"Name","value":"login"}}},{"kind":"ObjectField","name":{"kind":"Name","value":"passwd"},"value":{"kind":"Variable","name":{"kind":"Name","value":"passwd"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"query"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"currentUserId"}}]}}]}}]}}]} as unknown as DocumentNode<SignInMutation, SignInMutationVariables>;
export const GetMenuDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetMenu"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"parameterByPaName"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"paName"},"value":{"kind":"StringValue","value":"menu","block":false}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"paValue"}}]}}]}}]} as unknown as DocumentNode<GetMenuQuery, GetMenuQueryVariables>;
export const userQueryQueryMock: ExecutionResult<UserQueryQuery> = { data: { getCurrentUser: { permissionByUGroup: { peAkce: 84, peAnkety: -28, peAktuality: -96, peDescription: 'Hello World', peDokumenty: 23, peGalerie: 84, peId: 'BigInt', peKonzole: -16, peInzerce: 79, peNabidka: 29, peMain: 18, peName: 'Hello World', peNastenka: 94, peNovinky: 81, pePary: 76, pePermissions: -49, pePlatby: -70, peRozpis: 28, peSkupiny: 22, peUsers: 19 }, uId: 'BigInt', uJmeno: 'Hello World', uPrijmeni: 'Hello World' } } };

export const upozorneniListQueryMock: ExecutionResult<UpozorneniListQuery> = { data: { allUpozornenis: { nodes: [{ upId: 'BigInt', upKdo: 'BigInt', upLock: false, upNadpis: 'Hello World', upText: 'Hello World', upTimestamp: 'Datetime', upTimestampAdd: 'Datetime', userByUpKdo: { uId: 'BigInt', uJmeno: 'Hello World', uPrijmeni: 'Hello World' }, upozorneniSkupiniesByUpsIdRodic: { nodes: [{ skupinyByUpsIdSkupina: { sName: 'Hello World', sDescription: 'Hello World', sColorText: 'Hello World', sColorRgb: 'Hello World' } }] } }, { upId: 'BigInt', upKdo: 'BigInt', upLock: true, upNadpis: 'Hello World', upText: 'Hello World', upTimestamp: 'Datetime', upTimestampAdd: 'Datetime', userByUpKdo: { uId: 'BigInt', uJmeno: 'Hello World', uPrijmeni: 'Hello World' }, upozorneniSkupiniesByUpsIdRodic: { nodes: [{ skupinyByUpsIdSkupina: { sName: 'Hello World', sDescription: 'Hello World', sColorText: 'Hello World', sColorRgb: 'Hello World' } }, { skupinyByUpsIdSkupina: { sName: 'Hello World', sDescription: 'Hello World', sColorText: 'Hello World', sColorRgb: 'Hello World' } }] } }], totalCount: 59 } } };

export const articlesAdminListQueryMock: ExecutionResult<ArticlesAdminListQuery> = { data: { allAktualities: { nodes: [{ atFoto: 'BigInt', atFotoMain: 'BigInt', atId: 'BigInt', atJmeno: 'Hello World', atKdo: 'BigInt', atPreview: 'Hello World', atText: 'Hello World', atTimestampAdd: 'Datetime', atTimestamp: 'Datetime' }, { atFoto: 'BigInt', atFotoMain: 'BigInt', atId: 'BigInt', atJmeno: 'Hello World', atKdo: 'BigInt', atPreview: 'Hello World', atText: 'Hello World', atTimestampAdd: 'Datetime', atTimestamp: 'Datetime' }, { atFoto: 'BigInt', atFotoMain: 'BigInt', atId: 'BigInt', atJmeno: 'Hello World', atKdo: 'BigInt', atPreview: 'Hello World', atText: 'Hello World', atTimestampAdd: 'Datetime', atTimestamp: 'Datetime' }], totalCount: -11 } } };

export const akceListQueryMock: ExecutionResult<AkceListQuery> = { data: { allAkces: { nodes: [{ aDo: 'Date', aId: 'BigInt', aInfo: 'Hello World', aDokumenty: 'Hello World', aJmeno: 'Hello World', aKapacita: 'BigInt', aKde: 'Hello World', aLock: true, aOd: 'Date', aTimestamp: 'Datetime', aVisible: false, akceItemsByAiIdRodic: { nodes: [{ aiId: 'BigInt', userByAiUser: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } }], totalCount: -54 } }], totalCount: -97 } } };

export const setAkceVisibleMutationMock: ExecutionResult<SetAkceVisibleMutation> = { data: { updateAkceByAId: { akce: { aId: 'BigInt' } } } };

export const galleryDirListQueryMock: ExecutionResult<GalleryDirListQuery> = { data: { allGalerieDirs: { nodes: [{ gdHidden: true, gdId: 'BigInt', gdIdRodic: 'BigInt', gdLevel: 38, gdName: 'Hello World', gdPath: 'Hello World' }, { gdHidden: false, gdId: 'BigInt', gdIdRodic: 'BigInt', gdLevel: 28, gdName: 'Hello World', gdPath: 'Hello World' }], totalCount: -12 } } };

export const setGalerieDirVisibleMutationMock: ExecutionResult<SetGalerieDirVisibleMutation> = { data: { updateGalerieDirByGdId: { galerieDir: { gdId: 'BigInt' } } } };

export const reservationAdminListQueryMock: ExecutionResult<ReservationAdminListQuery> = { data: { allNabidkas: { nodes: [{ nDo: 'Date', nId: 'BigInt', nLock: false, nMaxPocetHod: 'BigInt', nOd: 'Date', nPocetHod: -62, nTimestamp: 'Datetime', nTrener: 'BigInt', nVisible: true, nabidkaItemsByNiIdRodic: { nodes: [{ niPocetHod: -7, niPartner: 'BigInt', niLock: true, paryByNiPartner: { userByPIdPartner: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } } }, { niPocetHod: 68, niPartner: 'BigInt', niLock: true, paryByNiPartner: { userByPIdPartner: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } } }, { niPocetHod: 22, niPartner: 'BigInt', niLock: false, paryByNiPartner: { userByPIdPartner: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } } }] }, userByNTrener: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } }], totalCount: 84 } } };

export const setNabidkaVisibleMutationMock: ExecutionResult<SetNabidkaVisibleMutation> = { data: { updateNabidkaByNId: { nabidka: { nId: 'BigInt' } } } };

export const nabidkaListQueryMock: ExecutionResult<NabidkaListQuery> = { data: { allNabidkas: { nodes: [{ nDo: 'Date', nId: 'BigInt', nLock: true, nMaxPocetHod: 'BigInt', nOd: 'Date', nPocetHod: -86, nTimestamp: 'Datetime', nTrener: 'BigInt', nVisible: true, nabidkaItemsByNiIdRodic: { nodes: [{ niPocetHod: 82, niPartner: 'BigInt', niLock: false, paryByNiPartner: { userByPIdPartner: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } } }] }, userByNTrener: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } }, { nDo: 'Date', nId: 'BigInt', nLock: true, nMaxPocetHod: 'BigInt', nOd: 'Date', nPocetHod: 28, nTimestamp: 'Datetime', nTrener: 'BigInt', nVisible: false, nabidkaItemsByNiIdRodic: { nodes: [{ niPocetHod: 36, niPartner: 'BigInt', niLock: true, paryByNiPartner: { userByPIdPartner: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } } }] }, userByNTrener: { uJmeno: 'Hello World', uPrijmeni: 'Hello World', uId: 'BigInt' } }], totalCount: 36 } } };

export const scheduleAdminListQueryMock: ExecutionResult<ScheduleAdminListQuery> = { data: { allRozpis: { nodes: [{ rDatum: 'Date', rId: 'BigInt', rKde: 'Hello World', rLock: false, rTimestamp: 'Datetime', rTrener: 'BigInt', rVisible: false, userByRTrener: { uId: 'BigInt', uJmeno: 'Hello World', uPrijmeni: 'Hello World' }, rozpisItemsByRiIdRodic: { nodes: [{ riDo: 'Time', riOd: 'Time', riId: 'BigInt', riPartner: 'BigInt' }] } }], totalCount: -40 } } };

export const setRozpisVisibleMutationMock: ExecutionResult<SetRozpisVisibleMutation> = { data: { updateRozpiByRId: { rozpi: { rId: 'BigInt' } } } };

export const getPageQueryMock: ExecutionResult<GetPageQuery> = { data: { pageByUrl: { content: 'JSON' } } };

export const getPhotoQueryMock: ExecutionResult<GetPhotoQuery> = { data: { galerieFotoByGfId: { gfId: 'BigInt', gfIdRodic: 'BigInt', gfKdo: 'BigInt', gfName: 'Hello World', gfPath: 'Hello World', gfTimestamp: 'Datetime', galerieDirByGfIdRodic: { gdId: 'BigInt' } } } };

export const signInMutationMock: ExecutionResult<SignInMutation> = { data: { login: { query: { currentUserId: 'BigInt' } } } };

export const getMenuQueryMock: ExecutionResult<GetMenuQuery> = { data: { parameterByPaName: { paValue: 'Hello World' } } };