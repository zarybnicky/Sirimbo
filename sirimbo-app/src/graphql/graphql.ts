/* eslint-disable */
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
  bigint: any;
  bpchar: any;
  bytea: any;
  date: any;
  numeric: any;
  pary_p_lat_trida: any;
  pary_p_stt_trida: any;
  smallint: any;
  time: any;
  timestamptz: any;
};

/** Boolean expression to compare columns of type "Boolean". All fields are combined with logical 'AND'. */
export type Boolean_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['Boolean']>;
  _gt?: InputMaybe<Scalars['Boolean']>;
  _gte?: InputMaybe<Scalars['Boolean']>;
  _in?: InputMaybe<Array<Scalars['Boolean']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['Boolean']>;
  _lte?: InputMaybe<Scalars['Boolean']>;
  _neq?: InputMaybe<Scalars['Boolean']>;
  _nin?: InputMaybe<Array<Scalars['Boolean']>>;
};

/** Boolean expression to compare columns of type "Int". All fields are combined with logical 'AND'. */
export type Int_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['Int']>;
  _gt?: InputMaybe<Scalars['Int']>;
  _gte?: InputMaybe<Scalars['Int']>;
  _in?: InputMaybe<Array<Scalars['Int']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['Int']>;
  _lte?: InputMaybe<Scalars['Int']>;
  _neq?: InputMaybe<Scalars['Int']>;
  _nin?: InputMaybe<Array<Scalars['Int']>>;
};

/** Boolean expression to compare columns of type "String". All fields are combined with logical 'AND'. */
export type String_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['String']>;
  _gt?: InputMaybe<Scalars['String']>;
  _gte?: InputMaybe<Scalars['String']>;
  /** does the column match the given case-insensitive pattern */
  _ilike?: InputMaybe<Scalars['String']>;
  _in?: InputMaybe<Array<Scalars['String']>>;
  /** does the column match the given POSIX regular expression, case insensitive */
  _iregex?: InputMaybe<Scalars['String']>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  /** does the column match the given pattern */
  _like?: InputMaybe<Scalars['String']>;
  _lt?: InputMaybe<Scalars['String']>;
  _lte?: InputMaybe<Scalars['String']>;
  _neq?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given case-insensitive pattern */
  _nilike?: InputMaybe<Scalars['String']>;
  _nin?: InputMaybe<Array<Scalars['String']>>;
  /** does the column NOT match the given POSIX regular expression, case insensitive */
  _niregex?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given pattern */
  _nlike?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given POSIX regular expression, case sensitive */
  _nregex?: InputMaybe<Scalars['String']>;
  /** does the column NOT match the given SQL regular expression */
  _nsimilar?: InputMaybe<Scalars['String']>;
  /** does the column match the given POSIX regular expression, case sensitive */
  _regex?: InputMaybe<Scalars['String']>;
  /** does the column match the given SQL regular expression */
  _similar?: InputMaybe<Scalars['String']>;
};

/** columns and relationships of "akce" */
export type Akce = {
  __typename?: 'akce';
  a_do: Scalars['date'];
  a_dokumenty: Scalars['String'];
  a_id: Scalars['bigint'];
  a_info: Scalars['String'];
  a_jmeno: Scalars['String'];
  a_kapacita: Scalars['bigint'];
  a_kde: Scalars['String'];
  a_lock: Scalars['Boolean'];
  a_od: Scalars['date'];
  a_timestamp?: Maybe<Scalars['timestamptz']>;
  a_visible: Scalars['Boolean'];
  /** An array relationship */
  akce_items: Array<Akce_Item>;
  /** An aggregate relationship */
  akce_items_aggregate: Akce_Item_Aggregate;
};


/** columns and relationships of "akce" */
export type AkceAkce_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


/** columns and relationships of "akce" */
export type AkceAkce_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};

/** aggregated selection of "akce" */
export type Akce_Aggregate = {
  __typename?: 'akce_aggregate';
  aggregate?: Maybe<Akce_Aggregate_Fields>;
  nodes: Array<Akce>;
};

/** aggregate fields of "akce" */
export type Akce_Aggregate_Fields = {
  __typename?: 'akce_aggregate_fields';
  avg?: Maybe<Akce_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Akce_Max_Fields>;
  min?: Maybe<Akce_Min_Fields>;
  stddev?: Maybe<Akce_Stddev_Fields>;
  stddev_pop?: Maybe<Akce_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Akce_Stddev_Samp_Fields>;
  sum?: Maybe<Akce_Sum_Fields>;
  var_pop?: Maybe<Akce_Var_Pop_Fields>;
  var_samp?: Maybe<Akce_Var_Samp_Fields>;
  variance?: Maybe<Akce_Variance_Fields>;
};


/** aggregate fields of "akce" */
export type Akce_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Akce_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Akce_Avg_Fields = {
  __typename?: 'akce_avg_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "akce". All fields are combined with a logical 'AND'. */
export type Akce_Bool_Exp = {
  _and?: InputMaybe<Array<Akce_Bool_Exp>>;
  _not?: InputMaybe<Akce_Bool_Exp>;
  _or?: InputMaybe<Array<Akce_Bool_Exp>>;
  a_do?: InputMaybe<Date_Comparison_Exp>;
  a_dokumenty?: InputMaybe<String_Comparison_Exp>;
  a_id?: InputMaybe<Bigint_Comparison_Exp>;
  a_info?: InputMaybe<String_Comparison_Exp>;
  a_jmeno?: InputMaybe<String_Comparison_Exp>;
  a_kapacita?: InputMaybe<Bigint_Comparison_Exp>;
  a_kde?: InputMaybe<String_Comparison_Exp>;
  a_lock?: InputMaybe<Boolean_Comparison_Exp>;
  a_od?: InputMaybe<Date_Comparison_Exp>;
  a_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  a_visible?: InputMaybe<Boolean_Comparison_Exp>;
  akce_items?: InputMaybe<Akce_Item_Bool_Exp>;
};

/** unique or primary key constraints on table "akce" */
export enum Akce_Constraint {
  /** unique or primary key constraint */
  Idx_24557Primary = 'idx_24557_primary'
}

/** input type for incrementing numeric columns in table "akce" */
export type Akce_Inc_Input = {
  a_id?: InputMaybe<Scalars['bigint']>;
  a_kapacita?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "akce" */
export type Akce_Insert_Input = {
  a_do?: InputMaybe<Scalars['date']>;
  a_dokumenty?: InputMaybe<Scalars['String']>;
  a_id?: InputMaybe<Scalars['bigint']>;
  a_info?: InputMaybe<Scalars['String']>;
  a_jmeno?: InputMaybe<Scalars['String']>;
  a_kapacita?: InputMaybe<Scalars['bigint']>;
  a_kde?: InputMaybe<Scalars['String']>;
  a_lock?: InputMaybe<Scalars['Boolean']>;
  a_od?: InputMaybe<Scalars['date']>;
  a_timestamp?: InputMaybe<Scalars['timestamptz']>;
  a_visible?: InputMaybe<Scalars['Boolean']>;
  akce_items?: InputMaybe<Akce_Item_Arr_Rel_Insert_Input>;
};

/** columns and relationships of "akce_item" */
export type Akce_Item = {
  __typename?: 'akce_item';
  ai_id: Scalars['bigint'];
  ai_id_rodic: Scalars['bigint'];
  ai_rok_narozeni: Scalars['smallint'];
  ai_user: Scalars['bigint'];
  /** An object relationship */
  akce: Akce;
  /** An object relationship */
  user: Users;
};

/** aggregated selection of "akce_item" */
export type Akce_Item_Aggregate = {
  __typename?: 'akce_item_aggregate';
  aggregate?: Maybe<Akce_Item_Aggregate_Fields>;
  nodes: Array<Akce_Item>;
};

/** aggregate fields of "akce_item" */
export type Akce_Item_Aggregate_Fields = {
  __typename?: 'akce_item_aggregate_fields';
  avg?: Maybe<Akce_Item_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Akce_Item_Max_Fields>;
  min?: Maybe<Akce_Item_Min_Fields>;
  stddev?: Maybe<Akce_Item_Stddev_Fields>;
  stddev_pop?: Maybe<Akce_Item_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Akce_Item_Stddev_Samp_Fields>;
  sum?: Maybe<Akce_Item_Sum_Fields>;
  var_pop?: Maybe<Akce_Item_Var_Pop_Fields>;
  var_samp?: Maybe<Akce_Item_Var_Samp_Fields>;
  variance?: Maybe<Akce_Item_Variance_Fields>;
};


/** aggregate fields of "akce_item" */
export type Akce_Item_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Akce_Item_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "akce_item" */
export type Akce_Item_Aggregate_Order_By = {
  avg?: InputMaybe<Akce_Item_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Akce_Item_Max_Order_By>;
  min?: InputMaybe<Akce_Item_Min_Order_By>;
  stddev?: InputMaybe<Akce_Item_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Akce_Item_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Akce_Item_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Akce_Item_Sum_Order_By>;
  var_pop?: InputMaybe<Akce_Item_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Akce_Item_Var_Samp_Order_By>;
  variance?: InputMaybe<Akce_Item_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "akce_item" */
export type Akce_Item_Arr_Rel_Insert_Input = {
  data: Array<Akce_Item_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Akce_Item_On_Conflict>;
};

/** aggregate avg on columns */
export type Akce_Item_Avg_Fields = {
  __typename?: 'akce_item_avg_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "akce_item" */
export type Akce_Item_Avg_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "akce_item". All fields are combined with a logical 'AND'. */
export type Akce_Item_Bool_Exp = {
  _and?: InputMaybe<Array<Akce_Item_Bool_Exp>>;
  _not?: InputMaybe<Akce_Item_Bool_Exp>;
  _or?: InputMaybe<Array<Akce_Item_Bool_Exp>>;
  ai_id?: InputMaybe<Bigint_Comparison_Exp>;
  ai_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  ai_rok_narozeni?: InputMaybe<Smallint_Comparison_Exp>;
  ai_user?: InputMaybe<Bigint_Comparison_Exp>;
  akce?: InputMaybe<Akce_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "akce_item" */
export enum Akce_Item_Constraint {
  /** unique or primary key constraint */
  Idx_24569Primary = 'idx_24569_primary'
}

/** input type for incrementing numeric columns in table "akce_item" */
export type Akce_Item_Inc_Input = {
  ai_id?: InputMaybe<Scalars['bigint']>;
  ai_id_rodic?: InputMaybe<Scalars['bigint']>;
  ai_rok_narozeni?: InputMaybe<Scalars['smallint']>;
  ai_user?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "akce_item" */
export type Akce_Item_Insert_Input = {
  ai_id?: InputMaybe<Scalars['bigint']>;
  ai_id_rodic?: InputMaybe<Scalars['bigint']>;
  ai_rok_narozeni?: InputMaybe<Scalars['smallint']>;
  ai_user?: InputMaybe<Scalars['bigint']>;
  akce?: InputMaybe<Akce_Obj_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Akce_Item_Max_Fields = {
  __typename?: 'akce_item_max_fields';
  ai_id?: Maybe<Scalars['bigint']>;
  ai_id_rodic?: Maybe<Scalars['bigint']>;
  ai_rok_narozeni?: Maybe<Scalars['smallint']>;
  ai_user?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "akce_item" */
export type Akce_Item_Max_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Akce_Item_Min_Fields = {
  __typename?: 'akce_item_min_fields';
  ai_id?: Maybe<Scalars['bigint']>;
  ai_id_rodic?: Maybe<Scalars['bigint']>;
  ai_rok_narozeni?: Maybe<Scalars['smallint']>;
  ai_user?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "akce_item" */
export type Akce_Item_Min_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "akce_item" */
export type Akce_Item_Mutation_Response = {
  __typename?: 'akce_item_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Akce_Item>;
};

/** on conflict condition type for table "akce_item" */
export type Akce_Item_On_Conflict = {
  constraint: Akce_Item_Constraint;
  update_columns?: Array<Akce_Item_Update_Column>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};

/** Ordering options when selecting data from "akce_item". */
export type Akce_Item_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
  akce?: InputMaybe<Akce_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: akce_item */
export type Akce_Item_Pk_Columns_Input = {
  ai_id: Scalars['bigint'];
};

/** select columns of table "akce_item" */
export enum Akce_Item_Select_Column {
  /** column name */
  AiId = 'ai_id',
  /** column name */
  AiIdRodic = 'ai_id_rodic',
  /** column name */
  AiRokNarozeni = 'ai_rok_narozeni',
  /** column name */
  AiUser = 'ai_user'
}

/** input type for updating data in table "akce_item" */
export type Akce_Item_Set_Input = {
  ai_id?: InputMaybe<Scalars['bigint']>;
  ai_id_rodic?: InputMaybe<Scalars['bigint']>;
  ai_rok_narozeni?: InputMaybe<Scalars['smallint']>;
  ai_user?: InputMaybe<Scalars['bigint']>;
};

/** aggregate stddev on columns */
export type Akce_Item_Stddev_Fields = {
  __typename?: 'akce_item_stddev_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "akce_item" */
export type Akce_Item_Stddev_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Akce_Item_Stddev_Pop_Fields = {
  __typename?: 'akce_item_stddev_pop_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "akce_item" */
export type Akce_Item_Stddev_Pop_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Akce_Item_Stddev_Samp_Fields = {
  __typename?: 'akce_item_stddev_samp_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "akce_item" */
export type Akce_Item_Stddev_Samp_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Akce_Item_Sum_Fields = {
  __typename?: 'akce_item_sum_fields';
  ai_id?: Maybe<Scalars['bigint']>;
  ai_id_rodic?: Maybe<Scalars['bigint']>;
  ai_rok_narozeni?: Maybe<Scalars['smallint']>;
  ai_user?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "akce_item" */
export type Akce_Item_Sum_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** update columns of table "akce_item" */
export enum Akce_Item_Update_Column {
  /** column name */
  AiId = 'ai_id',
  /** column name */
  AiIdRodic = 'ai_id_rodic',
  /** column name */
  AiRokNarozeni = 'ai_rok_narozeni',
  /** column name */
  AiUser = 'ai_user'
}

/** aggregate var_pop on columns */
export type Akce_Item_Var_Pop_Fields = {
  __typename?: 'akce_item_var_pop_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "akce_item" */
export type Akce_Item_Var_Pop_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Akce_Item_Var_Samp_Fields = {
  __typename?: 'akce_item_var_samp_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "akce_item" */
export type Akce_Item_Var_Samp_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Akce_Item_Variance_Fields = {
  __typename?: 'akce_item_variance_fields';
  ai_id?: Maybe<Scalars['Float']>;
  ai_id_rodic?: Maybe<Scalars['Float']>;
  ai_rok_narozeni?: Maybe<Scalars['Float']>;
  ai_user?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "akce_item" */
export type Akce_Item_Variance_Order_By = {
  ai_id?: InputMaybe<Order_By>;
  ai_id_rodic?: InputMaybe<Order_By>;
  ai_rok_narozeni?: InputMaybe<Order_By>;
  ai_user?: InputMaybe<Order_By>;
};

/** aggregate max on columns */
export type Akce_Max_Fields = {
  __typename?: 'akce_max_fields';
  a_do?: Maybe<Scalars['date']>;
  a_dokumenty?: Maybe<Scalars['String']>;
  a_id?: Maybe<Scalars['bigint']>;
  a_info?: Maybe<Scalars['String']>;
  a_jmeno?: Maybe<Scalars['String']>;
  a_kapacita?: Maybe<Scalars['bigint']>;
  a_kde?: Maybe<Scalars['String']>;
  a_od?: Maybe<Scalars['date']>;
  a_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** aggregate min on columns */
export type Akce_Min_Fields = {
  __typename?: 'akce_min_fields';
  a_do?: Maybe<Scalars['date']>;
  a_dokumenty?: Maybe<Scalars['String']>;
  a_id?: Maybe<Scalars['bigint']>;
  a_info?: Maybe<Scalars['String']>;
  a_jmeno?: Maybe<Scalars['String']>;
  a_kapacita?: Maybe<Scalars['bigint']>;
  a_kde?: Maybe<Scalars['String']>;
  a_od?: Maybe<Scalars['date']>;
  a_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** response of any mutation on the table "akce" */
export type Akce_Mutation_Response = {
  __typename?: 'akce_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Akce>;
};

/** input type for inserting object relation for remote table "akce" */
export type Akce_Obj_Rel_Insert_Input = {
  data: Akce_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Akce_On_Conflict>;
};

/** on conflict condition type for table "akce" */
export type Akce_On_Conflict = {
  constraint: Akce_Constraint;
  update_columns?: Array<Akce_Update_Column>;
  where?: InputMaybe<Akce_Bool_Exp>;
};

/** Ordering options when selecting data from "akce". */
export type Akce_Order_By = {
  a_do?: InputMaybe<Order_By>;
  a_dokumenty?: InputMaybe<Order_By>;
  a_id?: InputMaybe<Order_By>;
  a_info?: InputMaybe<Order_By>;
  a_jmeno?: InputMaybe<Order_By>;
  a_kapacita?: InputMaybe<Order_By>;
  a_kde?: InputMaybe<Order_By>;
  a_lock?: InputMaybe<Order_By>;
  a_od?: InputMaybe<Order_By>;
  a_timestamp?: InputMaybe<Order_By>;
  a_visible?: InputMaybe<Order_By>;
  akce_items_aggregate?: InputMaybe<Akce_Item_Aggregate_Order_By>;
};

/** primary key columns input for table: akce */
export type Akce_Pk_Columns_Input = {
  a_id: Scalars['bigint'];
};

/** select columns of table "akce" */
export enum Akce_Select_Column {
  /** column name */
  ADo = 'a_do',
  /** column name */
  ADokumenty = 'a_dokumenty',
  /** column name */
  AId = 'a_id',
  /** column name */
  AInfo = 'a_info',
  /** column name */
  AJmeno = 'a_jmeno',
  /** column name */
  AKapacita = 'a_kapacita',
  /** column name */
  AKde = 'a_kde',
  /** column name */
  ALock = 'a_lock',
  /** column name */
  AOd = 'a_od',
  /** column name */
  ATimestamp = 'a_timestamp',
  /** column name */
  AVisible = 'a_visible'
}

/** input type for updating data in table "akce" */
export type Akce_Set_Input = {
  a_do?: InputMaybe<Scalars['date']>;
  a_dokumenty?: InputMaybe<Scalars['String']>;
  a_id?: InputMaybe<Scalars['bigint']>;
  a_info?: InputMaybe<Scalars['String']>;
  a_jmeno?: InputMaybe<Scalars['String']>;
  a_kapacita?: InputMaybe<Scalars['bigint']>;
  a_kde?: InputMaybe<Scalars['String']>;
  a_lock?: InputMaybe<Scalars['Boolean']>;
  a_od?: InputMaybe<Scalars['date']>;
  a_timestamp?: InputMaybe<Scalars['timestamptz']>;
  a_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Akce_Stddev_Fields = {
  __typename?: 'akce_stddev_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Akce_Stddev_Pop_Fields = {
  __typename?: 'akce_stddev_pop_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Akce_Stddev_Samp_Fields = {
  __typename?: 'akce_stddev_samp_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Akce_Sum_Fields = {
  __typename?: 'akce_sum_fields';
  a_id?: Maybe<Scalars['bigint']>;
  a_kapacita?: Maybe<Scalars['bigint']>;
};

/** update columns of table "akce" */
export enum Akce_Update_Column {
  /** column name */
  ADo = 'a_do',
  /** column name */
  ADokumenty = 'a_dokumenty',
  /** column name */
  AId = 'a_id',
  /** column name */
  AInfo = 'a_info',
  /** column name */
  AJmeno = 'a_jmeno',
  /** column name */
  AKapacita = 'a_kapacita',
  /** column name */
  AKde = 'a_kde',
  /** column name */
  ALock = 'a_lock',
  /** column name */
  AOd = 'a_od',
  /** column name */
  ATimestamp = 'a_timestamp',
  /** column name */
  AVisible = 'a_visible'
}

/** aggregate var_pop on columns */
export type Akce_Var_Pop_Fields = {
  __typename?: 'akce_var_pop_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Akce_Var_Samp_Fields = {
  __typename?: 'akce_var_samp_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Akce_Variance_Fields = {
  __typename?: 'akce_variance_fields';
  a_id?: Maybe<Scalars['Float']>;
  a_kapacita?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "aktuality" */
export type Aktuality = {
  __typename?: 'aktuality';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id: Scalars['bigint'];
  at_jmeno: Scalars['String'];
  at_kat: Scalars['String'];
  at_kdo: Scalars['bigint'];
  at_preview: Scalars['String'];
  at_text: Scalars['String'];
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
  /** An object relationship */
  galerie_foto?: Maybe<Galerie_Foto>;
  /** An object relationship */
  user: Users;
};

/** columns and relationships of "aktuality_admin" */
export type Aktuality_Admin = {
  __typename?: 'aktuality_admin';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_jmeno?: Maybe<Scalars['String']>;
  at_kat?: Maybe<Scalars['String']>;
  at_kdo?: Maybe<Scalars['bigint']>;
  at_preview?: Maybe<Scalars['String']>;
  at_text?: Maybe<Scalars['String']>;
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** aggregated selection of "aktuality_admin" */
export type Aktuality_Admin_Aggregate = {
  __typename?: 'aktuality_admin_aggregate';
  aggregate?: Maybe<Aktuality_Admin_Aggregate_Fields>;
  nodes: Array<Aktuality_Admin>;
};

/** aggregate fields of "aktuality_admin" */
export type Aktuality_Admin_Aggregate_Fields = {
  __typename?: 'aktuality_admin_aggregate_fields';
  avg?: Maybe<Aktuality_Admin_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Aktuality_Admin_Max_Fields>;
  min?: Maybe<Aktuality_Admin_Min_Fields>;
  stddev?: Maybe<Aktuality_Admin_Stddev_Fields>;
  stddev_pop?: Maybe<Aktuality_Admin_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Aktuality_Admin_Stddev_Samp_Fields>;
  sum?: Maybe<Aktuality_Admin_Sum_Fields>;
  var_pop?: Maybe<Aktuality_Admin_Var_Pop_Fields>;
  var_samp?: Maybe<Aktuality_Admin_Var_Samp_Fields>;
  variance?: Maybe<Aktuality_Admin_Variance_Fields>;
};


/** aggregate fields of "aktuality_admin" */
export type Aktuality_Admin_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Aktuality_Admin_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Aktuality_Admin_Avg_Fields = {
  __typename?: 'aktuality_admin_avg_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "aktuality_admin". All fields are combined with a logical 'AND'. */
export type Aktuality_Admin_Bool_Exp = {
  _and?: InputMaybe<Array<Aktuality_Admin_Bool_Exp>>;
  _not?: InputMaybe<Aktuality_Admin_Bool_Exp>;
  _or?: InputMaybe<Array<Aktuality_Admin_Bool_Exp>>;
  at_foto?: InputMaybe<Bigint_Comparison_Exp>;
  at_foto_main?: InputMaybe<Bigint_Comparison_Exp>;
  at_id?: InputMaybe<Bigint_Comparison_Exp>;
  at_jmeno?: InputMaybe<String_Comparison_Exp>;
  at_kat?: InputMaybe<String_Comparison_Exp>;
  at_kdo?: InputMaybe<Bigint_Comparison_Exp>;
  at_preview?: InputMaybe<String_Comparison_Exp>;
  at_text?: InputMaybe<String_Comparison_Exp>;
  at_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  at_timestamp_add?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** input type for incrementing numeric columns in table "aktuality_admin" */
export type Aktuality_Admin_Inc_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "aktuality_admin" */
export type Aktuality_Admin_Insert_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_jmeno?: InputMaybe<Scalars['String']>;
  at_kat?: InputMaybe<Scalars['String']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
  at_preview?: InputMaybe<Scalars['String']>;
  at_text?: InputMaybe<Scalars['String']>;
  at_timestamp?: InputMaybe<Scalars['timestamptz']>;
  at_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type Aktuality_Admin_Max_Fields = {
  __typename?: 'aktuality_admin_max_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_jmeno?: Maybe<Scalars['String']>;
  at_kat?: Maybe<Scalars['String']>;
  at_kdo?: Maybe<Scalars['bigint']>;
  at_preview?: Maybe<Scalars['String']>;
  at_text?: Maybe<Scalars['String']>;
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** aggregate min on columns */
export type Aktuality_Admin_Min_Fields = {
  __typename?: 'aktuality_admin_min_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_jmeno?: Maybe<Scalars['String']>;
  at_kat?: Maybe<Scalars['String']>;
  at_kdo?: Maybe<Scalars['bigint']>;
  at_preview?: Maybe<Scalars['String']>;
  at_text?: Maybe<Scalars['String']>;
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** response of any mutation on the table "aktuality_admin" */
export type Aktuality_Admin_Mutation_Response = {
  __typename?: 'aktuality_admin_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Aktuality_Admin>;
};

/** Ordering options when selecting data from "aktuality_admin". */
export type Aktuality_Admin_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_jmeno?: InputMaybe<Order_By>;
  at_kat?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
  at_preview?: InputMaybe<Order_By>;
  at_text?: InputMaybe<Order_By>;
  at_timestamp?: InputMaybe<Order_By>;
  at_timestamp_add?: InputMaybe<Order_By>;
};

/** select columns of table "aktuality_admin" */
export enum Aktuality_Admin_Select_Column {
  /** column name */
  AtFoto = 'at_foto',
  /** column name */
  AtFotoMain = 'at_foto_main',
  /** column name */
  AtId = 'at_id',
  /** column name */
  AtJmeno = 'at_jmeno',
  /** column name */
  AtKat = 'at_kat',
  /** column name */
  AtKdo = 'at_kdo',
  /** column name */
  AtPreview = 'at_preview',
  /** column name */
  AtText = 'at_text',
  /** column name */
  AtTimestamp = 'at_timestamp',
  /** column name */
  AtTimestampAdd = 'at_timestamp_add'
}

/** input type for updating data in table "aktuality_admin" */
export type Aktuality_Admin_Set_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_jmeno?: InputMaybe<Scalars['String']>;
  at_kat?: InputMaybe<Scalars['String']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
  at_preview?: InputMaybe<Scalars['String']>;
  at_text?: InputMaybe<Scalars['String']>;
  at_timestamp?: InputMaybe<Scalars['timestamptz']>;
  at_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Aktuality_Admin_Stddev_Fields = {
  __typename?: 'aktuality_admin_stddev_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Aktuality_Admin_Stddev_Pop_Fields = {
  __typename?: 'aktuality_admin_stddev_pop_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Aktuality_Admin_Stddev_Samp_Fields = {
  __typename?: 'aktuality_admin_stddev_samp_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Aktuality_Admin_Sum_Fields = {
  __typename?: 'aktuality_admin_sum_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_kdo?: Maybe<Scalars['bigint']>;
};

/** aggregate var_pop on columns */
export type Aktuality_Admin_Var_Pop_Fields = {
  __typename?: 'aktuality_admin_var_pop_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Aktuality_Admin_Var_Samp_Fields = {
  __typename?: 'aktuality_admin_var_samp_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Aktuality_Admin_Variance_Fields = {
  __typename?: 'aktuality_admin_variance_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** aggregated selection of "aktuality" */
export type Aktuality_Aggregate = {
  __typename?: 'aktuality_aggregate';
  aggregate?: Maybe<Aktuality_Aggregate_Fields>;
  nodes: Array<Aktuality>;
};

/** aggregate fields of "aktuality" */
export type Aktuality_Aggregate_Fields = {
  __typename?: 'aktuality_aggregate_fields';
  avg?: Maybe<Aktuality_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Aktuality_Max_Fields>;
  min?: Maybe<Aktuality_Min_Fields>;
  stddev?: Maybe<Aktuality_Stddev_Fields>;
  stddev_pop?: Maybe<Aktuality_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Aktuality_Stddev_Samp_Fields>;
  sum?: Maybe<Aktuality_Sum_Fields>;
  var_pop?: Maybe<Aktuality_Var_Pop_Fields>;
  var_samp?: Maybe<Aktuality_Var_Samp_Fields>;
  variance?: Maybe<Aktuality_Variance_Fields>;
};


/** aggregate fields of "aktuality" */
export type Aktuality_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Aktuality_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "aktuality" */
export type Aktuality_Aggregate_Order_By = {
  avg?: InputMaybe<Aktuality_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Aktuality_Max_Order_By>;
  min?: InputMaybe<Aktuality_Min_Order_By>;
  stddev?: InputMaybe<Aktuality_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Aktuality_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Aktuality_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Aktuality_Sum_Order_By>;
  var_pop?: InputMaybe<Aktuality_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Aktuality_Var_Samp_Order_By>;
  variance?: InputMaybe<Aktuality_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "aktuality" */
export type Aktuality_Arr_Rel_Insert_Input = {
  data: Array<Aktuality_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Aktuality_On_Conflict>;
};

/** aggregate avg on columns */
export type Aktuality_Avg_Fields = {
  __typename?: 'aktuality_avg_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "aktuality" */
export type Aktuality_Avg_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "aktuality". All fields are combined with a logical 'AND'. */
export type Aktuality_Bool_Exp = {
  _and?: InputMaybe<Array<Aktuality_Bool_Exp>>;
  _not?: InputMaybe<Aktuality_Bool_Exp>;
  _or?: InputMaybe<Array<Aktuality_Bool_Exp>>;
  at_foto?: InputMaybe<Bigint_Comparison_Exp>;
  at_foto_main?: InputMaybe<Bigint_Comparison_Exp>;
  at_id?: InputMaybe<Bigint_Comparison_Exp>;
  at_jmeno?: InputMaybe<String_Comparison_Exp>;
  at_kat?: InputMaybe<String_Comparison_Exp>;
  at_kdo?: InputMaybe<Bigint_Comparison_Exp>;
  at_preview?: InputMaybe<String_Comparison_Exp>;
  at_text?: InputMaybe<String_Comparison_Exp>;
  at_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  at_timestamp_add?: InputMaybe<Timestamptz_Comparison_Exp>;
  galerie_foto?: InputMaybe<Galerie_Foto_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "aktuality" */
export enum Aktuality_Constraint {
  /** unique or primary key constraint */
  Idx_24575Primary = 'idx_24575_primary'
}

/** input type for incrementing numeric columns in table "aktuality" */
export type Aktuality_Inc_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "aktuality" */
export type Aktuality_Insert_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_jmeno?: InputMaybe<Scalars['String']>;
  at_kat?: InputMaybe<Scalars['String']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
  at_preview?: InputMaybe<Scalars['String']>;
  at_text?: InputMaybe<Scalars['String']>;
  at_timestamp?: InputMaybe<Scalars['timestamptz']>;
  at_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
  galerie_foto?: InputMaybe<Galerie_Foto_Obj_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Aktuality_Max_Fields = {
  __typename?: 'aktuality_max_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_jmeno?: Maybe<Scalars['String']>;
  at_kat?: Maybe<Scalars['String']>;
  at_kdo?: Maybe<Scalars['bigint']>;
  at_preview?: Maybe<Scalars['String']>;
  at_text?: Maybe<Scalars['String']>;
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "aktuality" */
export type Aktuality_Max_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_jmeno?: InputMaybe<Order_By>;
  at_kat?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
  at_preview?: InputMaybe<Order_By>;
  at_text?: InputMaybe<Order_By>;
  at_timestamp?: InputMaybe<Order_By>;
  at_timestamp_add?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Aktuality_Min_Fields = {
  __typename?: 'aktuality_min_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_jmeno?: Maybe<Scalars['String']>;
  at_kat?: Maybe<Scalars['String']>;
  at_kdo?: Maybe<Scalars['bigint']>;
  at_preview?: Maybe<Scalars['String']>;
  at_text?: Maybe<Scalars['String']>;
  at_timestamp?: Maybe<Scalars['timestamptz']>;
  at_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "aktuality" */
export type Aktuality_Min_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_jmeno?: InputMaybe<Order_By>;
  at_kat?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
  at_preview?: InputMaybe<Order_By>;
  at_text?: InputMaybe<Order_By>;
  at_timestamp?: InputMaybe<Order_By>;
  at_timestamp_add?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "aktuality" */
export type Aktuality_Mutation_Response = {
  __typename?: 'aktuality_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Aktuality>;
};

/** on conflict condition type for table "aktuality" */
export type Aktuality_On_Conflict = {
  constraint: Aktuality_Constraint;
  update_columns?: Array<Aktuality_Update_Column>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};

/** Ordering options when selecting data from "aktuality". */
export type Aktuality_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_jmeno?: InputMaybe<Order_By>;
  at_kat?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
  at_preview?: InputMaybe<Order_By>;
  at_text?: InputMaybe<Order_By>;
  at_timestamp?: InputMaybe<Order_By>;
  at_timestamp_add?: InputMaybe<Order_By>;
  galerie_foto?: InputMaybe<Galerie_Foto_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: aktuality */
export type Aktuality_Pk_Columns_Input = {
  at_id: Scalars['bigint'];
};

/** select columns of table "aktuality" */
export enum Aktuality_Select_Column {
  /** column name */
  AtFoto = 'at_foto',
  /** column name */
  AtFotoMain = 'at_foto_main',
  /** column name */
  AtId = 'at_id',
  /** column name */
  AtJmeno = 'at_jmeno',
  /** column name */
  AtKat = 'at_kat',
  /** column name */
  AtKdo = 'at_kdo',
  /** column name */
  AtPreview = 'at_preview',
  /** column name */
  AtText = 'at_text',
  /** column name */
  AtTimestamp = 'at_timestamp',
  /** column name */
  AtTimestampAdd = 'at_timestamp_add'
}

/** input type for updating data in table "aktuality" */
export type Aktuality_Set_Input = {
  at_foto?: InputMaybe<Scalars['bigint']>;
  at_foto_main?: InputMaybe<Scalars['bigint']>;
  at_id?: InputMaybe<Scalars['bigint']>;
  at_jmeno?: InputMaybe<Scalars['String']>;
  at_kat?: InputMaybe<Scalars['String']>;
  at_kdo?: InputMaybe<Scalars['bigint']>;
  at_preview?: InputMaybe<Scalars['String']>;
  at_text?: InputMaybe<Scalars['String']>;
  at_timestamp?: InputMaybe<Scalars['timestamptz']>;
  at_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Aktuality_Stddev_Fields = {
  __typename?: 'aktuality_stddev_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "aktuality" */
export type Aktuality_Stddev_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Aktuality_Stddev_Pop_Fields = {
  __typename?: 'aktuality_stddev_pop_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "aktuality" */
export type Aktuality_Stddev_Pop_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Aktuality_Stddev_Samp_Fields = {
  __typename?: 'aktuality_stddev_samp_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "aktuality" */
export type Aktuality_Stddev_Samp_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Aktuality_Sum_Fields = {
  __typename?: 'aktuality_sum_fields';
  at_foto?: Maybe<Scalars['bigint']>;
  at_foto_main?: Maybe<Scalars['bigint']>;
  at_id?: Maybe<Scalars['bigint']>;
  at_kdo?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "aktuality" */
export type Aktuality_Sum_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** update columns of table "aktuality" */
export enum Aktuality_Update_Column {
  /** column name */
  AtFoto = 'at_foto',
  /** column name */
  AtFotoMain = 'at_foto_main',
  /** column name */
  AtId = 'at_id',
  /** column name */
  AtJmeno = 'at_jmeno',
  /** column name */
  AtKat = 'at_kat',
  /** column name */
  AtKdo = 'at_kdo',
  /** column name */
  AtPreview = 'at_preview',
  /** column name */
  AtText = 'at_text',
  /** column name */
  AtTimestamp = 'at_timestamp',
  /** column name */
  AtTimestampAdd = 'at_timestamp_add'
}

/** aggregate var_pop on columns */
export type Aktuality_Var_Pop_Fields = {
  __typename?: 'aktuality_var_pop_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "aktuality" */
export type Aktuality_Var_Pop_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Aktuality_Var_Samp_Fields = {
  __typename?: 'aktuality_var_samp_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "aktuality" */
export type Aktuality_Var_Samp_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Aktuality_Variance_Fields = {
  __typename?: 'aktuality_variance_fields';
  at_foto?: Maybe<Scalars['Float']>;
  at_foto_main?: Maybe<Scalars['Float']>;
  at_id?: Maybe<Scalars['Float']>;
  at_kdo?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "aktuality" */
export type Aktuality_Variance_Order_By = {
  at_foto?: InputMaybe<Order_By>;
  at_foto_main?: InputMaybe<Order_By>;
  at_id?: InputMaybe<Order_By>;
  at_kdo?: InputMaybe<Order_By>;
};

/** Boolean expression to compare columns of type "bigint". All fields are combined with logical 'AND'. */
export type Bigint_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['bigint']>;
  _gt?: InputMaybe<Scalars['bigint']>;
  _gte?: InputMaybe<Scalars['bigint']>;
  _in?: InputMaybe<Array<Scalars['bigint']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['bigint']>;
  _lte?: InputMaybe<Scalars['bigint']>;
  _neq?: InputMaybe<Scalars['bigint']>;
  _nin?: InputMaybe<Array<Scalars['bigint']>>;
};

/** Boolean expression to compare columns of type "bpchar". All fields are combined with logical 'AND'. */
export type Bpchar_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['bpchar']>;
  _gt?: InputMaybe<Scalars['bpchar']>;
  _gte?: InputMaybe<Scalars['bpchar']>;
  /** does the column match the given case-insensitive pattern */
  _ilike?: InputMaybe<Scalars['bpchar']>;
  _in?: InputMaybe<Array<Scalars['bpchar']>>;
  /** does the column match the given POSIX regular expression, case insensitive */
  _iregex?: InputMaybe<Scalars['bpchar']>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  /** does the column match the given pattern */
  _like?: InputMaybe<Scalars['bpchar']>;
  _lt?: InputMaybe<Scalars['bpchar']>;
  _lte?: InputMaybe<Scalars['bpchar']>;
  _neq?: InputMaybe<Scalars['bpchar']>;
  /** does the column NOT match the given case-insensitive pattern */
  _nilike?: InputMaybe<Scalars['bpchar']>;
  _nin?: InputMaybe<Array<Scalars['bpchar']>>;
  /** does the column NOT match the given POSIX regular expression, case insensitive */
  _niregex?: InputMaybe<Scalars['bpchar']>;
  /** does the column NOT match the given pattern */
  _nlike?: InputMaybe<Scalars['bpchar']>;
  /** does the column NOT match the given POSIX regular expression, case sensitive */
  _nregex?: InputMaybe<Scalars['bpchar']>;
  /** does the column NOT match the given SQL regular expression */
  _nsimilar?: InputMaybe<Scalars['bpchar']>;
  /** does the column match the given POSIX regular expression, case sensitive */
  _regex?: InputMaybe<Scalars['bpchar']>;
  /** does the column match the given SQL regular expression */
  _similar?: InputMaybe<Scalars['bpchar']>;
};

/** Boolean expression to compare columns of type "bytea". All fields are combined with logical 'AND'. */
export type Bytea_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['bytea']>;
  _gt?: InputMaybe<Scalars['bytea']>;
  _gte?: InputMaybe<Scalars['bytea']>;
  _in?: InputMaybe<Array<Scalars['bytea']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['bytea']>;
  _lte?: InputMaybe<Scalars['bytea']>;
  _neq?: InputMaybe<Scalars['bytea']>;
  _nin?: InputMaybe<Array<Scalars['bytea']>>;
};

/** Boolean expression to compare columns of type "date". All fields are combined with logical 'AND'. */
export type Date_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['date']>;
  _gt?: InputMaybe<Scalars['date']>;
  _gte?: InputMaybe<Scalars['date']>;
  _in?: InputMaybe<Array<Scalars['date']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['date']>;
  _lte?: InputMaybe<Scalars['date']>;
  _neq?: InputMaybe<Scalars['date']>;
  _nin?: InputMaybe<Array<Scalars['date']>>;
};

/** columns and relationships of "dokumenty" */
export type Dokumenty = {
  __typename?: 'dokumenty';
  d_filename: Scalars['String'];
  d_id: Scalars['bigint'];
  d_kategorie: Scalars['smallint'];
  d_kdo: Scalars['bigint'];
  d_name: Scalars['String'];
  d_path: Scalars['String'];
  d_timestamp?: Maybe<Scalars['timestamptz']>;
  /** An object relationship */
  user: Users;
};

/** aggregated selection of "dokumenty" */
export type Dokumenty_Aggregate = {
  __typename?: 'dokumenty_aggregate';
  aggregate?: Maybe<Dokumenty_Aggregate_Fields>;
  nodes: Array<Dokumenty>;
};

/** aggregate fields of "dokumenty" */
export type Dokumenty_Aggregate_Fields = {
  __typename?: 'dokumenty_aggregate_fields';
  avg?: Maybe<Dokumenty_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Dokumenty_Max_Fields>;
  min?: Maybe<Dokumenty_Min_Fields>;
  stddev?: Maybe<Dokumenty_Stddev_Fields>;
  stddev_pop?: Maybe<Dokumenty_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Dokumenty_Stddev_Samp_Fields>;
  sum?: Maybe<Dokumenty_Sum_Fields>;
  var_pop?: Maybe<Dokumenty_Var_Pop_Fields>;
  var_samp?: Maybe<Dokumenty_Var_Samp_Fields>;
  variance?: Maybe<Dokumenty_Variance_Fields>;
};


/** aggregate fields of "dokumenty" */
export type Dokumenty_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Dokumenty_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "dokumenty" */
export type Dokumenty_Aggregate_Order_By = {
  avg?: InputMaybe<Dokumenty_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Dokumenty_Max_Order_By>;
  min?: InputMaybe<Dokumenty_Min_Order_By>;
  stddev?: InputMaybe<Dokumenty_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Dokumenty_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Dokumenty_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Dokumenty_Sum_Order_By>;
  var_pop?: InputMaybe<Dokumenty_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Dokumenty_Var_Samp_Order_By>;
  variance?: InputMaybe<Dokumenty_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "dokumenty" */
export type Dokumenty_Arr_Rel_Insert_Input = {
  data: Array<Dokumenty_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Dokumenty_On_Conflict>;
};

/** aggregate avg on columns */
export type Dokumenty_Avg_Fields = {
  __typename?: 'dokumenty_avg_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "dokumenty" */
export type Dokumenty_Avg_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "dokumenty". All fields are combined with a logical 'AND'. */
export type Dokumenty_Bool_Exp = {
  _and?: InputMaybe<Array<Dokumenty_Bool_Exp>>;
  _not?: InputMaybe<Dokumenty_Bool_Exp>;
  _or?: InputMaybe<Array<Dokumenty_Bool_Exp>>;
  d_filename?: InputMaybe<String_Comparison_Exp>;
  d_id?: InputMaybe<Bigint_Comparison_Exp>;
  d_kategorie?: InputMaybe<Smallint_Comparison_Exp>;
  d_kdo?: InputMaybe<Bigint_Comparison_Exp>;
  d_name?: InputMaybe<String_Comparison_Exp>;
  d_path?: InputMaybe<String_Comparison_Exp>;
  d_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "dokumenty" */
export enum Dokumenty_Constraint {
  /** unique or primary key constraint */
  Idx_24593DPath = 'idx_24593_d_path',
  /** unique or primary key constraint */
  Idx_24593Primary = 'idx_24593_primary'
}

/** input type for incrementing numeric columns in table "dokumenty" */
export type Dokumenty_Inc_Input = {
  d_id?: InputMaybe<Scalars['bigint']>;
  d_kategorie?: InputMaybe<Scalars['smallint']>;
  d_kdo?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "dokumenty" */
export type Dokumenty_Insert_Input = {
  d_filename?: InputMaybe<Scalars['String']>;
  d_id?: InputMaybe<Scalars['bigint']>;
  d_kategorie?: InputMaybe<Scalars['smallint']>;
  d_kdo?: InputMaybe<Scalars['bigint']>;
  d_name?: InputMaybe<Scalars['String']>;
  d_path?: InputMaybe<Scalars['String']>;
  d_timestamp?: InputMaybe<Scalars['timestamptz']>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Dokumenty_Max_Fields = {
  __typename?: 'dokumenty_max_fields';
  d_filename?: Maybe<Scalars['String']>;
  d_id?: Maybe<Scalars['bigint']>;
  d_kategorie?: Maybe<Scalars['smallint']>;
  d_kdo?: Maybe<Scalars['bigint']>;
  d_name?: Maybe<Scalars['String']>;
  d_path?: Maybe<Scalars['String']>;
  d_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "dokumenty" */
export type Dokumenty_Max_Order_By = {
  d_filename?: InputMaybe<Order_By>;
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
  d_name?: InputMaybe<Order_By>;
  d_path?: InputMaybe<Order_By>;
  d_timestamp?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Dokumenty_Min_Fields = {
  __typename?: 'dokumenty_min_fields';
  d_filename?: Maybe<Scalars['String']>;
  d_id?: Maybe<Scalars['bigint']>;
  d_kategorie?: Maybe<Scalars['smallint']>;
  d_kdo?: Maybe<Scalars['bigint']>;
  d_name?: Maybe<Scalars['String']>;
  d_path?: Maybe<Scalars['String']>;
  d_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "dokumenty" */
export type Dokumenty_Min_Order_By = {
  d_filename?: InputMaybe<Order_By>;
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
  d_name?: InputMaybe<Order_By>;
  d_path?: InputMaybe<Order_By>;
  d_timestamp?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "dokumenty" */
export type Dokumenty_Mutation_Response = {
  __typename?: 'dokumenty_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Dokumenty>;
};

/** on conflict condition type for table "dokumenty" */
export type Dokumenty_On_Conflict = {
  constraint: Dokumenty_Constraint;
  update_columns?: Array<Dokumenty_Update_Column>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};

/** Ordering options when selecting data from "dokumenty". */
export type Dokumenty_Order_By = {
  d_filename?: InputMaybe<Order_By>;
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
  d_name?: InputMaybe<Order_By>;
  d_path?: InputMaybe<Order_By>;
  d_timestamp?: InputMaybe<Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: dokumenty */
export type Dokumenty_Pk_Columns_Input = {
  d_id: Scalars['bigint'];
};

/** select columns of table "dokumenty" */
export enum Dokumenty_Select_Column {
  /** column name */
  DFilename = 'd_filename',
  /** column name */
  DId = 'd_id',
  /** column name */
  DKategorie = 'd_kategorie',
  /** column name */
  DKdo = 'd_kdo',
  /** column name */
  DName = 'd_name',
  /** column name */
  DPath = 'd_path',
  /** column name */
  DTimestamp = 'd_timestamp'
}

/** input type for updating data in table "dokumenty" */
export type Dokumenty_Set_Input = {
  d_filename?: InputMaybe<Scalars['String']>;
  d_id?: InputMaybe<Scalars['bigint']>;
  d_kategorie?: InputMaybe<Scalars['smallint']>;
  d_kdo?: InputMaybe<Scalars['bigint']>;
  d_name?: InputMaybe<Scalars['String']>;
  d_path?: InputMaybe<Scalars['String']>;
  d_timestamp?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Dokumenty_Stddev_Fields = {
  __typename?: 'dokumenty_stddev_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "dokumenty" */
export type Dokumenty_Stddev_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Dokumenty_Stddev_Pop_Fields = {
  __typename?: 'dokumenty_stddev_pop_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "dokumenty" */
export type Dokumenty_Stddev_Pop_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Dokumenty_Stddev_Samp_Fields = {
  __typename?: 'dokumenty_stddev_samp_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "dokumenty" */
export type Dokumenty_Stddev_Samp_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Dokumenty_Sum_Fields = {
  __typename?: 'dokumenty_sum_fields';
  d_id?: Maybe<Scalars['bigint']>;
  d_kategorie?: Maybe<Scalars['smallint']>;
  d_kdo?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "dokumenty" */
export type Dokumenty_Sum_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** update columns of table "dokumenty" */
export enum Dokumenty_Update_Column {
  /** column name */
  DFilename = 'd_filename',
  /** column name */
  DId = 'd_id',
  /** column name */
  DKategorie = 'd_kategorie',
  /** column name */
  DKdo = 'd_kdo',
  /** column name */
  DName = 'd_name',
  /** column name */
  DPath = 'd_path',
  /** column name */
  DTimestamp = 'd_timestamp'
}

/** aggregate var_pop on columns */
export type Dokumenty_Var_Pop_Fields = {
  __typename?: 'dokumenty_var_pop_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "dokumenty" */
export type Dokumenty_Var_Pop_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Dokumenty_Var_Samp_Fields = {
  __typename?: 'dokumenty_var_samp_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "dokumenty" */
export type Dokumenty_Var_Samp_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Dokumenty_Variance_Fields = {
  __typename?: 'dokumenty_variance_fields';
  d_id?: Maybe<Scalars['Float']>;
  d_kategorie?: Maybe<Scalars['Float']>;
  d_kdo?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "dokumenty" */
export type Dokumenty_Variance_Order_By = {
  d_id?: InputMaybe<Order_By>;
  d_kategorie?: InputMaybe<Order_By>;
  d_kdo?: InputMaybe<Order_By>;
};

/** columns and relationships of "galerie_dir" */
export type Galerie_Dir = {
  __typename?: 'galerie_dir';
  /** An array relationship */
  galerie_fotos: Array<Galerie_Foto>;
  /** An aggregate relationship */
  galerie_fotos_aggregate: Galerie_Foto_Aggregate;
  gd_hidden: Scalars['Boolean'];
  gd_id: Scalars['bigint'];
  gd_id_rodic: Scalars['bigint'];
  gd_level: Scalars['smallint'];
  gd_name: Scalars['String'];
  gd_path: Scalars['String'];
};


/** columns and relationships of "galerie_dir" */
export type Galerie_DirGalerie_FotosArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


/** columns and relationships of "galerie_dir" */
export type Galerie_DirGalerie_Fotos_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};

/** aggregated selection of "galerie_dir" */
export type Galerie_Dir_Aggregate = {
  __typename?: 'galerie_dir_aggregate';
  aggregate?: Maybe<Galerie_Dir_Aggregate_Fields>;
  nodes: Array<Galerie_Dir>;
};

/** aggregate fields of "galerie_dir" */
export type Galerie_Dir_Aggregate_Fields = {
  __typename?: 'galerie_dir_aggregate_fields';
  avg?: Maybe<Galerie_Dir_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Galerie_Dir_Max_Fields>;
  min?: Maybe<Galerie_Dir_Min_Fields>;
  stddev?: Maybe<Galerie_Dir_Stddev_Fields>;
  stddev_pop?: Maybe<Galerie_Dir_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Galerie_Dir_Stddev_Samp_Fields>;
  sum?: Maybe<Galerie_Dir_Sum_Fields>;
  var_pop?: Maybe<Galerie_Dir_Var_Pop_Fields>;
  var_samp?: Maybe<Galerie_Dir_Var_Samp_Fields>;
  variance?: Maybe<Galerie_Dir_Variance_Fields>;
};


/** aggregate fields of "galerie_dir" */
export type Galerie_Dir_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Galerie_Dir_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Galerie_Dir_Avg_Fields = {
  __typename?: 'galerie_dir_avg_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "galerie_dir". All fields are combined with a logical 'AND'. */
export type Galerie_Dir_Bool_Exp = {
  _and?: InputMaybe<Array<Galerie_Dir_Bool_Exp>>;
  _not?: InputMaybe<Galerie_Dir_Bool_Exp>;
  _or?: InputMaybe<Array<Galerie_Dir_Bool_Exp>>;
  galerie_fotos?: InputMaybe<Galerie_Foto_Bool_Exp>;
  gd_hidden?: InputMaybe<Boolean_Comparison_Exp>;
  gd_id?: InputMaybe<Bigint_Comparison_Exp>;
  gd_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  gd_level?: InputMaybe<Smallint_Comparison_Exp>;
  gd_name?: InputMaybe<String_Comparison_Exp>;
  gd_path?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "galerie_dir" */
export enum Galerie_Dir_Constraint {
  /** unique or primary key constraint */
  Idx_24602Primary = 'idx_24602_primary'
}

/** input type for incrementing numeric columns in table "galerie_dir" */
export type Galerie_Dir_Inc_Input = {
  gd_id?: InputMaybe<Scalars['bigint']>;
  gd_id_rodic?: InputMaybe<Scalars['bigint']>;
  gd_level?: InputMaybe<Scalars['smallint']>;
};

/** input type for inserting data into table "galerie_dir" */
export type Galerie_Dir_Insert_Input = {
  galerie_fotos?: InputMaybe<Galerie_Foto_Arr_Rel_Insert_Input>;
  gd_hidden?: InputMaybe<Scalars['Boolean']>;
  gd_id?: InputMaybe<Scalars['bigint']>;
  gd_id_rodic?: InputMaybe<Scalars['bigint']>;
  gd_level?: InputMaybe<Scalars['smallint']>;
  gd_name?: InputMaybe<Scalars['String']>;
  gd_path?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Galerie_Dir_Max_Fields = {
  __typename?: 'galerie_dir_max_fields';
  gd_id?: Maybe<Scalars['bigint']>;
  gd_id_rodic?: Maybe<Scalars['bigint']>;
  gd_level?: Maybe<Scalars['smallint']>;
  gd_name?: Maybe<Scalars['String']>;
  gd_path?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Galerie_Dir_Min_Fields = {
  __typename?: 'galerie_dir_min_fields';
  gd_id?: Maybe<Scalars['bigint']>;
  gd_id_rodic?: Maybe<Scalars['bigint']>;
  gd_level?: Maybe<Scalars['smallint']>;
  gd_name?: Maybe<Scalars['String']>;
  gd_path?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "galerie_dir" */
export type Galerie_Dir_Mutation_Response = {
  __typename?: 'galerie_dir_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Galerie_Dir>;
};

/** input type for inserting object relation for remote table "galerie_dir" */
export type Galerie_Dir_Obj_Rel_Insert_Input = {
  data: Galerie_Dir_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Galerie_Dir_On_Conflict>;
};

/** on conflict condition type for table "galerie_dir" */
export type Galerie_Dir_On_Conflict = {
  constraint: Galerie_Dir_Constraint;
  update_columns?: Array<Galerie_Dir_Update_Column>;
  where?: InputMaybe<Galerie_Dir_Bool_Exp>;
};

/** Ordering options when selecting data from "galerie_dir". */
export type Galerie_Dir_Order_By = {
  galerie_fotos_aggregate?: InputMaybe<Galerie_Foto_Aggregate_Order_By>;
  gd_hidden?: InputMaybe<Order_By>;
  gd_id?: InputMaybe<Order_By>;
  gd_id_rodic?: InputMaybe<Order_By>;
  gd_level?: InputMaybe<Order_By>;
  gd_name?: InputMaybe<Order_By>;
  gd_path?: InputMaybe<Order_By>;
};

/** primary key columns input for table: galerie_dir */
export type Galerie_Dir_Pk_Columns_Input = {
  gd_id: Scalars['bigint'];
};

/** select columns of table "galerie_dir" */
export enum Galerie_Dir_Select_Column {
  /** column name */
  GdHidden = 'gd_hidden',
  /** column name */
  GdId = 'gd_id',
  /** column name */
  GdIdRodic = 'gd_id_rodic',
  /** column name */
  GdLevel = 'gd_level',
  /** column name */
  GdName = 'gd_name',
  /** column name */
  GdPath = 'gd_path'
}

/** input type for updating data in table "galerie_dir" */
export type Galerie_Dir_Set_Input = {
  gd_hidden?: InputMaybe<Scalars['Boolean']>;
  gd_id?: InputMaybe<Scalars['bigint']>;
  gd_id_rodic?: InputMaybe<Scalars['bigint']>;
  gd_level?: InputMaybe<Scalars['smallint']>;
  gd_name?: InputMaybe<Scalars['String']>;
  gd_path?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Galerie_Dir_Stddev_Fields = {
  __typename?: 'galerie_dir_stddev_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Galerie_Dir_Stddev_Pop_Fields = {
  __typename?: 'galerie_dir_stddev_pop_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Galerie_Dir_Stddev_Samp_Fields = {
  __typename?: 'galerie_dir_stddev_samp_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Galerie_Dir_Sum_Fields = {
  __typename?: 'galerie_dir_sum_fields';
  gd_id?: Maybe<Scalars['bigint']>;
  gd_id_rodic?: Maybe<Scalars['bigint']>;
  gd_level?: Maybe<Scalars['smallint']>;
};

/** update columns of table "galerie_dir" */
export enum Galerie_Dir_Update_Column {
  /** column name */
  GdHidden = 'gd_hidden',
  /** column name */
  GdId = 'gd_id',
  /** column name */
  GdIdRodic = 'gd_id_rodic',
  /** column name */
  GdLevel = 'gd_level',
  /** column name */
  GdName = 'gd_name',
  /** column name */
  GdPath = 'gd_path'
}

/** aggregate var_pop on columns */
export type Galerie_Dir_Var_Pop_Fields = {
  __typename?: 'galerie_dir_var_pop_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Galerie_Dir_Var_Samp_Fields = {
  __typename?: 'galerie_dir_var_samp_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Galerie_Dir_Variance_Fields = {
  __typename?: 'galerie_dir_variance_fields';
  gd_id?: Maybe<Scalars['Float']>;
  gd_id_rodic?: Maybe<Scalars['Float']>;
  gd_level?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "galerie_foto" */
export type Galerie_Foto = {
  __typename?: 'galerie_foto';
  /** An array relationship */
  aktualities: Array<Aktuality>;
  /** An aggregate relationship */
  aktualities_aggregate: Aktuality_Aggregate;
  /** An object relationship */
  galerie_dir: Galerie_Dir;
  gf_id: Scalars['bigint'];
  gf_id_rodic: Scalars['bigint'];
  gf_kdo: Scalars['bigint'];
  gf_name: Scalars['String'];
  gf_path: Scalars['String'];
  gf_timestamp?: Maybe<Scalars['timestamptz']>;
  /** An object relationship */
  user: Users;
};


/** columns and relationships of "galerie_foto" */
export type Galerie_FotoAktualitiesArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


/** columns and relationships of "galerie_foto" */
export type Galerie_FotoAktualities_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};

/** aggregated selection of "galerie_foto" */
export type Galerie_Foto_Aggregate = {
  __typename?: 'galerie_foto_aggregate';
  aggregate?: Maybe<Galerie_Foto_Aggregate_Fields>;
  nodes: Array<Galerie_Foto>;
};

/** aggregate fields of "galerie_foto" */
export type Galerie_Foto_Aggregate_Fields = {
  __typename?: 'galerie_foto_aggregate_fields';
  avg?: Maybe<Galerie_Foto_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Galerie_Foto_Max_Fields>;
  min?: Maybe<Galerie_Foto_Min_Fields>;
  stddev?: Maybe<Galerie_Foto_Stddev_Fields>;
  stddev_pop?: Maybe<Galerie_Foto_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Galerie_Foto_Stddev_Samp_Fields>;
  sum?: Maybe<Galerie_Foto_Sum_Fields>;
  var_pop?: Maybe<Galerie_Foto_Var_Pop_Fields>;
  var_samp?: Maybe<Galerie_Foto_Var_Samp_Fields>;
  variance?: Maybe<Galerie_Foto_Variance_Fields>;
};


/** aggregate fields of "galerie_foto" */
export type Galerie_Foto_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "galerie_foto" */
export type Galerie_Foto_Aggregate_Order_By = {
  avg?: InputMaybe<Galerie_Foto_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Galerie_Foto_Max_Order_By>;
  min?: InputMaybe<Galerie_Foto_Min_Order_By>;
  stddev?: InputMaybe<Galerie_Foto_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Galerie_Foto_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Galerie_Foto_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Galerie_Foto_Sum_Order_By>;
  var_pop?: InputMaybe<Galerie_Foto_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Galerie_Foto_Var_Samp_Order_By>;
  variance?: InputMaybe<Galerie_Foto_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "galerie_foto" */
export type Galerie_Foto_Arr_Rel_Insert_Input = {
  data: Array<Galerie_Foto_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Galerie_Foto_On_Conflict>;
};

/** aggregate avg on columns */
export type Galerie_Foto_Avg_Fields = {
  __typename?: 'galerie_foto_avg_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "galerie_foto" */
export type Galerie_Foto_Avg_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "galerie_foto". All fields are combined with a logical 'AND'. */
export type Galerie_Foto_Bool_Exp = {
  _and?: InputMaybe<Array<Galerie_Foto_Bool_Exp>>;
  _not?: InputMaybe<Galerie_Foto_Bool_Exp>;
  _or?: InputMaybe<Array<Galerie_Foto_Bool_Exp>>;
  aktualities?: InputMaybe<Aktuality_Bool_Exp>;
  galerie_dir?: InputMaybe<Galerie_Dir_Bool_Exp>;
  gf_id?: InputMaybe<Bigint_Comparison_Exp>;
  gf_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  gf_kdo?: InputMaybe<Bigint_Comparison_Exp>;
  gf_name?: InputMaybe<String_Comparison_Exp>;
  gf_path?: InputMaybe<String_Comparison_Exp>;
  gf_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "galerie_foto" */
export enum Galerie_Foto_Constraint {
  /** unique or primary key constraint */
  Idx_24613Primary = 'idx_24613_primary'
}

/** input type for incrementing numeric columns in table "galerie_foto" */
export type Galerie_Foto_Inc_Input = {
  gf_id?: InputMaybe<Scalars['bigint']>;
  gf_id_rodic?: InputMaybe<Scalars['bigint']>;
  gf_kdo?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "galerie_foto" */
export type Galerie_Foto_Insert_Input = {
  aktualities?: InputMaybe<Aktuality_Arr_Rel_Insert_Input>;
  galerie_dir?: InputMaybe<Galerie_Dir_Obj_Rel_Insert_Input>;
  gf_id?: InputMaybe<Scalars['bigint']>;
  gf_id_rodic?: InputMaybe<Scalars['bigint']>;
  gf_kdo?: InputMaybe<Scalars['bigint']>;
  gf_name?: InputMaybe<Scalars['String']>;
  gf_path?: InputMaybe<Scalars['String']>;
  gf_timestamp?: InputMaybe<Scalars['timestamptz']>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Galerie_Foto_Max_Fields = {
  __typename?: 'galerie_foto_max_fields';
  gf_id?: Maybe<Scalars['bigint']>;
  gf_id_rodic?: Maybe<Scalars['bigint']>;
  gf_kdo?: Maybe<Scalars['bigint']>;
  gf_name?: Maybe<Scalars['String']>;
  gf_path?: Maybe<Scalars['String']>;
  gf_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "galerie_foto" */
export type Galerie_Foto_Max_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
  gf_name?: InputMaybe<Order_By>;
  gf_path?: InputMaybe<Order_By>;
  gf_timestamp?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Galerie_Foto_Min_Fields = {
  __typename?: 'galerie_foto_min_fields';
  gf_id?: Maybe<Scalars['bigint']>;
  gf_id_rodic?: Maybe<Scalars['bigint']>;
  gf_kdo?: Maybe<Scalars['bigint']>;
  gf_name?: Maybe<Scalars['String']>;
  gf_path?: Maybe<Scalars['String']>;
  gf_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "galerie_foto" */
export type Galerie_Foto_Min_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
  gf_name?: InputMaybe<Order_By>;
  gf_path?: InputMaybe<Order_By>;
  gf_timestamp?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "galerie_foto" */
export type Galerie_Foto_Mutation_Response = {
  __typename?: 'galerie_foto_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Galerie_Foto>;
};

/** input type for inserting object relation for remote table "galerie_foto" */
export type Galerie_Foto_Obj_Rel_Insert_Input = {
  data: Galerie_Foto_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Galerie_Foto_On_Conflict>;
};

/** on conflict condition type for table "galerie_foto" */
export type Galerie_Foto_On_Conflict = {
  constraint: Galerie_Foto_Constraint;
  update_columns?: Array<Galerie_Foto_Update_Column>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};

/** Ordering options when selecting data from "galerie_foto". */
export type Galerie_Foto_Order_By = {
  aktualities_aggregate?: InputMaybe<Aktuality_Aggregate_Order_By>;
  galerie_dir?: InputMaybe<Galerie_Dir_Order_By>;
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
  gf_name?: InputMaybe<Order_By>;
  gf_path?: InputMaybe<Order_By>;
  gf_timestamp?: InputMaybe<Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: galerie_foto */
export type Galerie_Foto_Pk_Columns_Input = {
  gf_id: Scalars['bigint'];
};

/** select columns of table "galerie_foto" */
export enum Galerie_Foto_Select_Column {
  /** column name */
  GfId = 'gf_id',
  /** column name */
  GfIdRodic = 'gf_id_rodic',
  /** column name */
  GfKdo = 'gf_kdo',
  /** column name */
  GfName = 'gf_name',
  /** column name */
  GfPath = 'gf_path',
  /** column name */
  GfTimestamp = 'gf_timestamp'
}

/** input type for updating data in table "galerie_foto" */
export type Galerie_Foto_Set_Input = {
  gf_id?: InputMaybe<Scalars['bigint']>;
  gf_id_rodic?: InputMaybe<Scalars['bigint']>;
  gf_kdo?: InputMaybe<Scalars['bigint']>;
  gf_name?: InputMaybe<Scalars['String']>;
  gf_path?: InputMaybe<Scalars['String']>;
  gf_timestamp?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Galerie_Foto_Stddev_Fields = {
  __typename?: 'galerie_foto_stddev_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "galerie_foto" */
export type Galerie_Foto_Stddev_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Galerie_Foto_Stddev_Pop_Fields = {
  __typename?: 'galerie_foto_stddev_pop_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "galerie_foto" */
export type Galerie_Foto_Stddev_Pop_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Galerie_Foto_Stddev_Samp_Fields = {
  __typename?: 'galerie_foto_stddev_samp_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "galerie_foto" */
export type Galerie_Foto_Stddev_Samp_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Galerie_Foto_Sum_Fields = {
  __typename?: 'galerie_foto_sum_fields';
  gf_id?: Maybe<Scalars['bigint']>;
  gf_id_rodic?: Maybe<Scalars['bigint']>;
  gf_kdo?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "galerie_foto" */
export type Galerie_Foto_Sum_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** update columns of table "galerie_foto" */
export enum Galerie_Foto_Update_Column {
  /** column name */
  GfId = 'gf_id',
  /** column name */
  GfIdRodic = 'gf_id_rodic',
  /** column name */
  GfKdo = 'gf_kdo',
  /** column name */
  GfName = 'gf_name',
  /** column name */
  GfPath = 'gf_path',
  /** column name */
  GfTimestamp = 'gf_timestamp'
}

/** aggregate var_pop on columns */
export type Galerie_Foto_Var_Pop_Fields = {
  __typename?: 'galerie_foto_var_pop_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "galerie_foto" */
export type Galerie_Foto_Var_Pop_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Galerie_Foto_Var_Samp_Fields = {
  __typename?: 'galerie_foto_var_samp_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "galerie_foto" */
export type Galerie_Foto_Var_Samp_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Galerie_Foto_Variance_Fields = {
  __typename?: 'galerie_foto_variance_fields';
  gf_id?: Maybe<Scalars['Float']>;
  gf_id_rodic?: Maybe<Scalars['Float']>;
  gf_kdo?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "galerie_foto" */
export type Galerie_Foto_Variance_Order_By = {
  gf_id?: InputMaybe<Order_By>;
  gf_id_rodic?: InputMaybe<Order_By>;
  gf_kdo?: InputMaybe<Order_By>;
};

/** mutation root */
export type Mutation_Root = {
  __typename?: 'mutation_root';
  /** delete data from the table: "akce" */
  delete_akce?: Maybe<Akce_Mutation_Response>;
  /** delete single row from the table: "akce" */
  delete_akce_by_pk?: Maybe<Akce>;
  /** delete data from the table: "akce_item" */
  delete_akce_item?: Maybe<Akce_Item_Mutation_Response>;
  /** delete single row from the table: "akce_item" */
  delete_akce_item_by_pk?: Maybe<Akce_Item>;
  /** delete data from the table: "aktuality" */
  delete_aktuality?: Maybe<Aktuality_Mutation_Response>;
  /** delete data from the table: "aktuality_admin" */
  delete_aktuality_admin?: Maybe<Aktuality_Admin_Mutation_Response>;
  /** delete single row from the table: "aktuality" */
  delete_aktuality_by_pk?: Maybe<Aktuality>;
  /** delete data from the table: "dokumenty" */
  delete_dokumenty?: Maybe<Dokumenty_Mutation_Response>;
  /** delete single row from the table: "dokumenty" */
  delete_dokumenty_by_pk?: Maybe<Dokumenty>;
  /** delete data from the table: "galerie_dir" */
  delete_galerie_dir?: Maybe<Galerie_Dir_Mutation_Response>;
  /** delete single row from the table: "galerie_dir" */
  delete_galerie_dir_by_pk?: Maybe<Galerie_Dir>;
  /** delete data from the table: "galerie_foto" */
  delete_galerie_foto?: Maybe<Galerie_Foto_Mutation_Response>;
  /** delete single row from the table: "galerie_foto" */
  delete_galerie_foto_by_pk?: Maybe<Galerie_Foto>;
  /** delete data from the table: "nabidka" */
  delete_nabidka?: Maybe<Nabidka_Mutation_Response>;
  /** delete data from the table: "nabidka_admin" */
  delete_nabidka_admin?: Maybe<Nabidka_Admin_Mutation_Response>;
  /** delete single row from the table: "nabidka" */
  delete_nabidka_by_pk?: Maybe<Nabidka>;
  /** delete data from the table: "nabidka_item" */
  delete_nabidka_item?: Maybe<Nabidka_Item_Mutation_Response>;
  /** delete single row from the table: "nabidka_item" */
  delete_nabidka_item_by_pk?: Maybe<Nabidka_Item>;
  /** delete data from the table: "parameters" */
  delete_parameters?: Maybe<Parameters_Mutation_Response>;
  /** delete single row from the table: "parameters" */
  delete_parameters_by_pk?: Maybe<Parameters>;
  /** delete data from the table: "pary" */
  delete_pary?: Maybe<Pary_Mutation_Response>;
  /** delete single row from the table: "pary" */
  delete_pary_by_pk?: Maybe<Pary>;
  /** delete data from the table: "pary_navrh" */
  delete_pary_navrh?: Maybe<Pary_Navrh_Mutation_Response>;
  /** delete single row from the table: "pary_navrh" */
  delete_pary_navrh_by_pk?: Maybe<Pary_Navrh>;
  /** delete data from the table: "permissions" */
  delete_permissions?: Maybe<Permissions_Mutation_Response>;
  /** delete single row from the table: "permissions" */
  delete_permissions_by_pk?: Maybe<Permissions>;
  /** delete data from the table: "platby_category" */
  delete_platby_category?: Maybe<Platby_Category_Mutation_Response>;
  /** delete single row from the table: "platby_category" */
  delete_platby_category_by_pk?: Maybe<Platby_Category>;
  /** delete data from the table: "platby_category_group" */
  delete_platby_category_group?: Maybe<Platby_Category_Group_Mutation_Response>;
  /** delete single row from the table: "platby_category_group" */
  delete_platby_category_group_by_pk?: Maybe<Platby_Category_Group>;
  /** delete data from the table: "platby_group" */
  delete_platby_group?: Maybe<Platby_Group_Mutation_Response>;
  /** delete single row from the table: "platby_group" */
  delete_platby_group_by_pk?: Maybe<Platby_Group>;
  /** delete data from the table: "platby_group_skupina" */
  delete_platby_group_skupina?: Maybe<Platby_Group_Skupina_Mutation_Response>;
  /** delete single row from the table: "platby_group_skupina" */
  delete_platby_group_skupina_by_pk?: Maybe<Platby_Group_Skupina>;
  /** delete data from the table: "platby_item" */
  delete_platby_item?: Maybe<Platby_Item_Mutation_Response>;
  /** delete single row from the table: "platby_item" */
  delete_platby_item_by_pk?: Maybe<Platby_Item>;
  /** delete data from the table: "platby_raw" */
  delete_platby_raw?: Maybe<Platby_Raw_Mutation_Response>;
  /** delete single row from the table: "platby_raw" */
  delete_platby_raw_by_pk?: Maybe<Platby_Raw>;
  /** delete data from the table: "rozpis" */
  delete_rozpis?: Maybe<Rozpis_Mutation_Response>;
  /** delete data from the table: "rozpis_admin" */
  delete_rozpis_admin?: Maybe<Rozpis_Admin_Mutation_Response>;
  /** delete single row from the table: "rozpis" */
  delete_rozpis_by_pk?: Maybe<Rozpis>;
  /** delete data from the table: "rozpis_item" */
  delete_rozpis_item?: Maybe<Rozpis_Item_Mutation_Response>;
  /** delete single row from the table: "rozpis_item" */
  delete_rozpis_item_by_pk?: Maybe<Rozpis_Item>;
  /** delete data from the table: "session" */
  delete_session?: Maybe<Session_Mutation_Response>;
  /** delete single row from the table: "session" */
  delete_session_by_pk?: Maybe<Session>;
  /** delete data from the table: "skupiny" */
  delete_skupiny?: Maybe<Skupiny_Mutation_Response>;
  /** delete single row from the table: "skupiny" */
  delete_skupiny_by_pk?: Maybe<Skupiny>;
  /** delete data from the table: "upozorneni" */
  delete_upozorneni?: Maybe<Upozorneni_Mutation_Response>;
  /** delete single row from the table: "upozorneni" */
  delete_upozorneni_by_pk?: Maybe<Upozorneni>;
  /** delete data from the table: "upozorneni_skupiny" */
  delete_upozorneni_skupiny?: Maybe<Upozorneni_Skupiny_Mutation_Response>;
  /** delete single row from the table: "upozorneni_skupiny" */
  delete_upozorneni_skupiny_by_pk?: Maybe<Upozorneni_Skupiny>;
  /** delete data from the table: "users" */
  delete_users?: Maybe<Users_Mutation_Response>;
  /** delete single row from the table: "users" */
  delete_users_by_pk?: Maybe<Users>;
  /** delete data from the table: "users_skupiny" */
  delete_users_skupiny?: Maybe<Users_Skupiny_Mutation_Response>;
  /** delete single row from the table: "users_skupiny" */
  delete_users_skupiny_by_pk?: Maybe<Users_Skupiny>;
  /** delete data from the table: "video" */
  delete_video?: Maybe<Video_Mutation_Response>;
  /** delete single row from the table: "video" */
  delete_video_by_pk?: Maybe<Video>;
  /** delete data from the table: "video_list" */
  delete_video_list?: Maybe<Video_List_Mutation_Response>;
  /** delete single row from the table: "video_list" */
  delete_video_list_by_pk?: Maybe<Video_List>;
  /** delete data from the table: "video_source" */
  delete_video_source?: Maybe<Video_Source_Mutation_Response>;
  /** delete single row from the table: "video_source" */
  delete_video_source_by_pk?: Maybe<Video_Source>;
  /** insert data into the table: "akce" */
  insert_akce?: Maybe<Akce_Mutation_Response>;
  /** insert data into the table: "akce_item" */
  insert_akce_item?: Maybe<Akce_Item_Mutation_Response>;
  /** insert a single row into the table: "akce_item" */
  insert_akce_item_one?: Maybe<Akce_Item>;
  /** insert a single row into the table: "akce" */
  insert_akce_one?: Maybe<Akce>;
  /** insert data into the table: "aktuality" */
  insert_aktuality?: Maybe<Aktuality_Mutation_Response>;
  /** insert data into the table: "aktuality_admin" */
  insert_aktuality_admin?: Maybe<Aktuality_Admin_Mutation_Response>;
  /** insert a single row into the table: "aktuality_admin" */
  insert_aktuality_admin_one?: Maybe<Aktuality_Admin>;
  /** insert a single row into the table: "aktuality" */
  insert_aktuality_one?: Maybe<Aktuality>;
  /** insert data into the table: "dokumenty" */
  insert_dokumenty?: Maybe<Dokumenty_Mutation_Response>;
  /** insert a single row into the table: "dokumenty" */
  insert_dokumenty_one?: Maybe<Dokumenty>;
  /** insert data into the table: "galerie_dir" */
  insert_galerie_dir?: Maybe<Galerie_Dir_Mutation_Response>;
  /** insert a single row into the table: "galerie_dir" */
  insert_galerie_dir_one?: Maybe<Galerie_Dir>;
  /** insert data into the table: "galerie_foto" */
  insert_galerie_foto?: Maybe<Galerie_Foto_Mutation_Response>;
  /** insert a single row into the table: "galerie_foto" */
  insert_galerie_foto_one?: Maybe<Galerie_Foto>;
  /** insert data into the table: "nabidka" */
  insert_nabidka?: Maybe<Nabidka_Mutation_Response>;
  /** insert data into the table: "nabidka_admin" */
  insert_nabidka_admin?: Maybe<Nabidka_Admin_Mutation_Response>;
  /** insert a single row into the table: "nabidka_admin" */
  insert_nabidka_admin_one?: Maybe<Nabidka_Admin>;
  /** insert data into the table: "nabidka_item" */
  insert_nabidka_item?: Maybe<Nabidka_Item_Mutation_Response>;
  /** insert a single row into the table: "nabidka_item" */
  insert_nabidka_item_one?: Maybe<Nabidka_Item>;
  /** insert a single row into the table: "nabidka" */
  insert_nabidka_one?: Maybe<Nabidka>;
  /** insert data into the table: "parameters" */
  insert_parameters?: Maybe<Parameters_Mutation_Response>;
  /** insert a single row into the table: "parameters" */
  insert_parameters_one?: Maybe<Parameters>;
  /** insert data into the table: "pary" */
  insert_pary?: Maybe<Pary_Mutation_Response>;
  /** insert data into the table: "pary_navrh" */
  insert_pary_navrh?: Maybe<Pary_Navrh_Mutation_Response>;
  /** insert a single row into the table: "pary_navrh" */
  insert_pary_navrh_one?: Maybe<Pary_Navrh>;
  /** insert a single row into the table: "pary" */
  insert_pary_one?: Maybe<Pary>;
  /** insert data into the table: "permissions" */
  insert_permissions?: Maybe<Permissions_Mutation_Response>;
  /** insert a single row into the table: "permissions" */
  insert_permissions_one?: Maybe<Permissions>;
  /** insert data into the table: "platby_category" */
  insert_platby_category?: Maybe<Platby_Category_Mutation_Response>;
  /** insert data into the table: "platby_category_group" */
  insert_platby_category_group?: Maybe<Platby_Category_Group_Mutation_Response>;
  /** insert a single row into the table: "platby_category_group" */
  insert_platby_category_group_one?: Maybe<Platby_Category_Group>;
  /** insert a single row into the table: "platby_category" */
  insert_platby_category_one?: Maybe<Platby_Category>;
  /** insert data into the table: "platby_group" */
  insert_platby_group?: Maybe<Platby_Group_Mutation_Response>;
  /** insert a single row into the table: "platby_group" */
  insert_platby_group_one?: Maybe<Platby_Group>;
  /** insert data into the table: "platby_group_skupina" */
  insert_platby_group_skupina?: Maybe<Platby_Group_Skupina_Mutation_Response>;
  /** insert a single row into the table: "platby_group_skupina" */
  insert_platby_group_skupina_one?: Maybe<Platby_Group_Skupina>;
  /** insert data into the table: "platby_item" */
  insert_platby_item?: Maybe<Platby_Item_Mutation_Response>;
  /** insert a single row into the table: "platby_item" */
  insert_platby_item_one?: Maybe<Platby_Item>;
  /** insert data into the table: "platby_raw" */
  insert_platby_raw?: Maybe<Platby_Raw_Mutation_Response>;
  /** insert a single row into the table: "platby_raw" */
  insert_platby_raw_one?: Maybe<Platby_Raw>;
  /** insert data into the table: "rozpis" */
  insert_rozpis?: Maybe<Rozpis_Mutation_Response>;
  /** insert data into the table: "rozpis_admin" */
  insert_rozpis_admin?: Maybe<Rozpis_Admin_Mutation_Response>;
  /** insert a single row into the table: "rozpis_admin" */
  insert_rozpis_admin_one?: Maybe<Rozpis_Admin>;
  /** insert data into the table: "rozpis_item" */
  insert_rozpis_item?: Maybe<Rozpis_Item_Mutation_Response>;
  /** insert a single row into the table: "rozpis_item" */
  insert_rozpis_item_one?: Maybe<Rozpis_Item>;
  /** insert a single row into the table: "rozpis" */
  insert_rozpis_one?: Maybe<Rozpis>;
  /** insert data into the table: "session" */
  insert_session?: Maybe<Session_Mutation_Response>;
  /** insert a single row into the table: "session" */
  insert_session_one?: Maybe<Session>;
  /** insert data into the table: "skupiny" */
  insert_skupiny?: Maybe<Skupiny_Mutation_Response>;
  /** insert a single row into the table: "skupiny" */
  insert_skupiny_one?: Maybe<Skupiny>;
  /** insert data into the table: "upozorneni" */
  insert_upozorneni?: Maybe<Upozorneni_Mutation_Response>;
  /** insert a single row into the table: "upozorneni" */
  insert_upozorneni_one?: Maybe<Upozorneni>;
  /** insert data into the table: "upozorneni_skupiny" */
  insert_upozorneni_skupiny?: Maybe<Upozorneni_Skupiny_Mutation_Response>;
  /** insert a single row into the table: "upozorneni_skupiny" */
  insert_upozorneni_skupiny_one?: Maybe<Upozorneni_Skupiny>;
  /** insert data into the table: "users" */
  insert_users?: Maybe<Users_Mutation_Response>;
  /** insert a single row into the table: "users" */
  insert_users_one?: Maybe<Users>;
  /** insert data into the table: "users_skupiny" */
  insert_users_skupiny?: Maybe<Users_Skupiny_Mutation_Response>;
  /** insert a single row into the table: "users_skupiny" */
  insert_users_skupiny_one?: Maybe<Users_Skupiny>;
  /** insert data into the table: "video" */
  insert_video?: Maybe<Video_Mutation_Response>;
  /** insert data into the table: "video_list" */
  insert_video_list?: Maybe<Video_List_Mutation_Response>;
  /** insert a single row into the table: "video_list" */
  insert_video_list_one?: Maybe<Video_List>;
  /** insert a single row into the table: "video" */
  insert_video_one?: Maybe<Video>;
  /** insert data into the table: "video_source" */
  insert_video_source?: Maybe<Video_Source_Mutation_Response>;
  /** insert a single row into the table: "video_source" */
  insert_video_source_one?: Maybe<Video_Source>;
  /** update data of the table: "akce" */
  update_akce?: Maybe<Akce_Mutation_Response>;
  /** update single row of the table: "akce" */
  update_akce_by_pk?: Maybe<Akce>;
  /** update data of the table: "akce_item" */
  update_akce_item?: Maybe<Akce_Item_Mutation_Response>;
  /** update single row of the table: "akce_item" */
  update_akce_item_by_pk?: Maybe<Akce_Item>;
  /** update data of the table: "aktuality" */
  update_aktuality?: Maybe<Aktuality_Mutation_Response>;
  /** update data of the table: "aktuality_admin" */
  update_aktuality_admin?: Maybe<Aktuality_Admin_Mutation_Response>;
  /** update single row of the table: "aktuality" */
  update_aktuality_by_pk?: Maybe<Aktuality>;
  /** update data of the table: "dokumenty" */
  update_dokumenty?: Maybe<Dokumenty_Mutation_Response>;
  /** update single row of the table: "dokumenty" */
  update_dokumenty_by_pk?: Maybe<Dokumenty>;
  /** update data of the table: "galerie_dir" */
  update_galerie_dir?: Maybe<Galerie_Dir_Mutation_Response>;
  /** update single row of the table: "galerie_dir" */
  update_galerie_dir_by_pk?: Maybe<Galerie_Dir>;
  /** update data of the table: "galerie_foto" */
  update_galerie_foto?: Maybe<Galerie_Foto_Mutation_Response>;
  /** update single row of the table: "galerie_foto" */
  update_galerie_foto_by_pk?: Maybe<Galerie_Foto>;
  /** update data of the table: "nabidka" */
  update_nabidka?: Maybe<Nabidka_Mutation_Response>;
  /** update data of the table: "nabidka_admin" */
  update_nabidka_admin?: Maybe<Nabidka_Admin_Mutation_Response>;
  /** update single row of the table: "nabidka" */
  update_nabidka_by_pk?: Maybe<Nabidka>;
  /** update data of the table: "nabidka_item" */
  update_nabidka_item?: Maybe<Nabidka_Item_Mutation_Response>;
  /** update single row of the table: "nabidka_item" */
  update_nabidka_item_by_pk?: Maybe<Nabidka_Item>;
  /** update data of the table: "parameters" */
  update_parameters?: Maybe<Parameters_Mutation_Response>;
  /** update single row of the table: "parameters" */
  update_parameters_by_pk?: Maybe<Parameters>;
  /** update data of the table: "pary" */
  update_pary?: Maybe<Pary_Mutation_Response>;
  /** update single row of the table: "pary" */
  update_pary_by_pk?: Maybe<Pary>;
  /** update data of the table: "pary_navrh" */
  update_pary_navrh?: Maybe<Pary_Navrh_Mutation_Response>;
  /** update single row of the table: "pary_navrh" */
  update_pary_navrh_by_pk?: Maybe<Pary_Navrh>;
  /** update data of the table: "permissions" */
  update_permissions?: Maybe<Permissions_Mutation_Response>;
  /** update single row of the table: "permissions" */
  update_permissions_by_pk?: Maybe<Permissions>;
  /** update data of the table: "platby_category" */
  update_platby_category?: Maybe<Platby_Category_Mutation_Response>;
  /** update single row of the table: "platby_category" */
  update_platby_category_by_pk?: Maybe<Platby_Category>;
  /** update data of the table: "platby_category_group" */
  update_platby_category_group?: Maybe<Platby_Category_Group_Mutation_Response>;
  /** update single row of the table: "platby_category_group" */
  update_platby_category_group_by_pk?: Maybe<Platby_Category_Group>;
  /** update data of the table: "platby_group" */
  update_platby_group?: Maybe<Platby_Group_Mutation_Response>;
  /** update single row of the table: "platby_group" */
  update_platby_group_by_pk?: Maybe<Platby_Group>;
  /** update data of the table: "platby_group_skupina" */
  update_platby_group_skupina?: Maybe<Platby_Group_Skupina_Mutation_Response>;
  /** update single row of the table: "platby_group_skupina" */
  update_platby_group_skupina_by_pk?: Maybe<Platby_Group_Skupina>;
  /** update data of the table: "platby_item" */
  update_platby_item?: Maybe<Platby_Item_Mutation_Response>;
  /** update single row of the table: "platby_item" */
  update_platby_item_by_pk?: Maybe<Platby_Item>;
  /** update data of the table: "platby_raw" */
  update_platby_raw?: Maybe<Platby_Raw_Mutation_Response>;
  /** update single row of the table: "platby_raw" */
  update_platby_raw_by_pk?: Maybe<Platby_Raw>;
  /** update data of the table: "rozpis" */
  update_rozpis?: Maybe<Rozpis_Mutation_Response>;
  /** update data of the table: "rozpis_admin" */
  update_rozpis_admin?: Maybe<Rozpis_Admin_Mutation_Response>;
  /** update single row of the table: "rozpis" */
  update_rozpis_by_pk?: Maybe<Rozpis>;
  /** update data of the table: "rozpis_item" */
  update_rozpis_item?: Maybe<Rozpis_Item_Mutation_Response>;
  /** update single row of the table: "rozpis_item" */
  update_rozpis_item_by_pk?: Maybe<Rozpis_Item>;
  /** update data of the table: "session" */
  update_session?: Maybe<Session_Mutation_Response>;
  /** update single row of the table: "session" */
  update_session_by_pk?: Maybe<Session>;
  /** update data of the table: "skupiny" */
  update_skupiny?: Maybe<Skupiny_Mutation_Response>;
  /** update single row of the table: "skupiny" */
  update_skupiny_by_pk?: Maybe<Skupiny>;
  /** update data of the table: "upozorneni" */
  update_upozorneni?: Maybe<Upozorneni_Mutation_Response>;
  /** update single row of the table: "upozorneni" */
  update_upozorneni_by_pk?: Maybe<Upozorneni>;
  /** update data of the table: "upozorneni_skupiny" */
  update_upozorneni_skupiny?: Maybe<Upozorneni_Skupiny_Mutation_Response>;
  /** update single row of the table: "upozorneni_skupiny" */
  update_upozorneni_skupiny_by_pk?: Maybe<Upozorneni_Skupiny>;
  /** update data of the table: "users" */
  update_users?: Maybe<Users_Mutation_Response>;
  /** update single row of the table: "users" */
  update_users_by_pk?: Maybe<Users>;
  /** update data of the table: "users_skupiny" */
  update_users_skupiny?: Maybe<Users_Skupiny_Mutation_Response>;
  /** update single row of the table: "users_skupiny" */
  update_users_skupiny_by_pk?: Maybe<Users_Skupiny>;
  /** update data of the table: "video" */
  update_video?: Maybe<Video_Mutation_Response>;
  /** update single row of the table: "video" */
  update_video_by_pk?: Maybe<Video>;
  /** update data of the table: "video_list" */
  update_video_list?: Maybe<Video_List_Mutation_Response>;
  /** update single row of the table: "video_list" */
  update_video_list_by_pk?: Maybe<Video_List>;
  /** update data of the table: "video_source" */
  update_video_source?: Maybe<Video_Source_Mutation_Response>;
  /** update single row of the table: "video_source" */
  update_video_source_by_pk?: Maybe<Video_Source>;
};


/** mutation root */
export type Mutation_RootDelete_AkceArgs = {
  where: Akce_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Akce_By_PkArgs = {
  a_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Akce_ItemArgs = {
  where: Akce_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Akce_Item_By_PkArgs = {
  ai_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_AktualityArgs = {
  where: Aktuality_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Aktuality_AdminArgs = {
  where: Aktuality_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Aktuality_By_PkArgs = {
  at_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_DokumentyArgs = {
  where: Dokumenty_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Dokumenty_By_PkArgs = {
  d_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Galerie_DirArgs = {
  where: Galerie_Dir_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Galerie_Dir_By_PkArgs = {
  gd_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Galerie_FotoArgs = {
  where: Galerie_Foto_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Galerie_Foto_By_PkArgs = {
  gf_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_NabidkaArgs = {
  where: Nabidka_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Nabidka_AdminArgs = {
  where: Nabidka_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Nabidka_By_PkArgs = {
  n_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Nabidka_ItemArgs = {
  where: Nabidka_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Nabidka_Item_By_PkArgs = {
  ni_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_ParametersArgs = {
  where: Parameters_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Parameters_By_PkArgs = {
  pa_name: Scalars['String'];
};


/** mutation root */
export type Mutation_RootDelete_ParyArgs = {
  where: Pary_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Pary_By_PkArgs = {
  p_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Pary_NavrhArgs = {
  where: Pary_Navrh_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Pary_Navrh_By_PkArgs = {
  pn_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_PermissionsArgs = {
  where: Permissions_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Permissions_By_PkArgs = {
  pe_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_CategoryArgs = {
  where: Platby_Category_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Category_By_PkArgs = {
  pc_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_Category_GroupArgs = {
  where: Platby_Category_Group_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Category_Group_By_PkArgs = {
  pcg_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_GroupArgs = {
  where: Platby_Group_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Group_By_PkArgs = {
  pg_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_Group_SkupinaArgs = {
  where: Platby_Group_Skupina_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Group_Skupina_By_PkArgs = {
  pgs_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_ItemArgs = {
  where: Platby_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Item_By_PkArgs = {
  pi_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Platby_RawArgs = {
  where: Platby_Raw_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Platby_Raw_By_PkArgs = {
  pr_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_RozpisArgs = {
  where: Rozpis_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Rozpis_AdminArgs = {
  where: Rozpis_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Rozpis_By_PkArgs = {
  r_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Rozpis_ItemArgs = {
  where: Rozpis_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Rozpis_Item_By_PkArgs = {
  ri_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_SessionArgs = {
  where: Session_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Session_By_PkArgs = {
  ss_id: Scalars['String'];
};


/** mutation root */
export type Mutation_RootDelete_SkupinyArgs = {
  where: Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Skupiny_By_PkArgs = {
  s_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_UpozorneniArgs = {
  where: Upozorneni_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Upozorneni_By_PkArgs = {
  up_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Upozorneni_SkupinyArgs = {
  where: Upozorneni_Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Upozorneni_Skupiny_By_PkArgs = {
  ups_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_UsersArgs = {
  where: Users_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Users_By_PkArgs = {
  u_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Users_SkupinyArgs = {
  where: Users_Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Users_Skupiny_By_PkArgs = {
  us_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_VideoArgs = {
  where: Video_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Video_By_PkArgs = {
  v_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Video_ListArgs = {
  where: Video_List_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Video_List_By_PkArgs = {
  vl_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootDelete_Video_SourceArgs = {
  where: Video_Source_Bool_Exp;
};


/** mutation root */
export type Mutation_RootDelete_Video_Source_By_PkArgs = {
  vs_id: Scalars['bigint'];
};


/** mutation root */
export type Mutation_RootInsert_AkceArgs = {
  objects: Array<Akce_Insert_Input>;
  on_conflict?: InputMaybe<Akce_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Akce_ItemArgs = {
  objects: Array<Akce_Item_Insert_Input>;
  on_conflict?: InputMaybe<Akce_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Akce_Item_OneArgs = {
  object: Akce_Item_Insert_Input;
  on_conflict?: InputMaybe<Akce_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Akce_OneArgs = {
  object: Akce_Insert_Input;
  on_conflict?: InputMaybe<Akce_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_AktualityArgs = {
  objects: Array<Aktuality_Insert_Input>;
  on_conflict?: InputMaybe<Aktuality_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Aktuality_AdminArgs = {
  objects: Array<Aktuality_Admin_Insert_Input>;
};


/** mutation root */
export type Mutation_RootInsert_Aktuality_Admin_OneArgs = {
  object: Aktuality_Admin_Insert_Input;
};


/** mutation root */
export type Mutation_RootInsert_Aktuality_OneArgs = {
  object: Aktuality_Insert_Input;
  on_conflict?: InputMaybe<Aktuality_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_DokumentyArgs = {
  objects: Array<Dokumenty_Insert_Input>;
  on_conflict?: InputMaybe<Dokumenty_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Dokumenty_OneArgs = {
  object: Dokumenty_Insert_Input;
  on_conflict?: InputMaybe<Dokumenty_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Galerie_DirArgs = {
  objects: Array<Galerie_Dir_Insert_Input>;
  on_conflict?: InputMaybe<Galerie_Dir_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Galerie_Dir_OneArgs = {
  object: Galerie_Dir_Insert_Input;
  on_conflict?: InputMaybe<Galerie_Dir_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Galerie_FotoArgs = {
  objects: Array<Galerie_Foto_Insert_Input>;
  on_conflict?: InputMaybe<Galerie_Foto_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Galerie_Foto_OneArgs = {
  object: Galerie_Foto_Insert_Input;
  on_conflict?: InputMaybe<Galerie_Foto_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_NabidkaArgs = {
  objects: Array<Nabidka_Insert_Input>;
  on_conflict?: InputMaybe<Nabidka_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Nabidka_AdminArgs = {
  objects: Array<Nabidka_Admin_Insert_Input>;
};


/** mutation root */
export type Mutation_RootInsert_Nabidka_Admin_OneArgs = {
  object: Nabidka_Admin_Insert_Input;
};


/** mutation root */
export type Mutation_RootInsert_Nabidka_ItemArgs = {
  objects: Array<Nabidka_Item_Insert_Input>;
  on_conflict?: InputMaybe<Nabidka_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Nabidka_Item_OneArgs = {
  object: Nabidka_Item_Insert_Input;
  on_conflict?: InputMaybe<Nabidka_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Nabidka_OneArgs = {
  object: Nabidka_Insert_Input;
  on_conflict?: InputMaybe<Nabidka_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_ParametersArgs = {
  objects: Array<Parameters_Insert_Input>;
  on_conflict?: InputMaybe<Parameters_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Parameters_OneArgs = {
  object: Parameters_Insert_Input;
  on_conflict?: InputMaybe<Parameters_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_ParyArgs = {
  objects: Array<Pary_Insert_Input>;
  on_conflict?: InputMaybe<Pary_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Pary_NavrhArgs = {
  objects: Array<Pary_Navrh_Insert_Input>;
  on_conflict?: InputMaybe<Pary_Navrh_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Pary_Navrh_OneArgs = {
  object: Pary_Navrh_Insert_Input;
  on_conflict?: InputMaybe<Pary_Navrh_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Pary_OneArgs = {
  object: Pary_Insert_Input;
  on_conflict?: InputMaybe<Pary_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_PermissionsArgs = {
  objects: Array<Permissions_Insert_Input>;
  on_conflict?: InputMaybe<Permissions_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Permissions_OneArgs = {
  object: Permissions_Insert_Input;
  on_conflict?: InputMaybe<Permissions_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_CategoryArgs = {
  objects: Array<Platby_Category_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Category_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Category_GroupArgs = {
  objects: Array<Platby_Category_Group_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Category_Group_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Category_Group_OneArgs = {
  object: Platby_Category_Group_Insert_Input;
  on_conflict?: InputMaybe<Platby_Category_Group_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Category_OneArgs = {
  object: Platby_Category_Insert_Input;
  on_conflict?: InputMaybe<Platby_Category_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_GroupArgs = {
  objects: Array<Platby_Group_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Group_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Group_OneArgs = {
  object: Platby_Group_Insert_Input;
  on_conflict?: InputMaybe<Platby_Group_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Group_SkupinaArgs = {
  objects: Array<Platby_Group_Skupina_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Group_Skupina_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Group_Skupina_OneArgs = {
  object: Platby_Group_Skupina_Insert_Input;
  on_conflict?: InputMaybe<Platby_Group_Skupina_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_ItemArgs = {
  objects: Array<Platby_Item_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Item_OneArgs = {
  object: Platby_Item_Insert_Input;
  on_conflict?: InputMaybe<Platby_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_RawArgs = {
  objects: Array<Platby_Raw_Insert_Input>;
  on_conflict?: InputMaybe<Platby_Raw_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Platby_Raw_OneArgs = {
  object: Platby_Raw_Insert_Input;
  on_conflict?: InputMaybe<Platby_Raw_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_RozpisArgs = {
  objects: Array<Rozpis_Insert_Input>;
  on_conflict?: InputMaybe<Rozpis_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Rozpis_AdminArgs = {
  objects: Array<Rozpis_Admin_Insert_Input>;
};


/** mutation root */
export type Mutation_RootInsert_Rozpis_Admin_OneArgs = {
  object: Rozpis_Admin_Insert_Input;
};


/** mutation root */
export type Mutation_RootInsert_Rozpis_ItemArgs = {
  objects: Array<Rozpis_Item_Insert_Input>;
  on_conflict?: InputMaybe<Rozpis_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Rozpis_Item_OneArgs = {
  object: Rozpis_Item_Insert_Input;
  on_conflict?: InputMaybe<Rozpis_Item_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Rozpis_OneArgs = {
  object: Rozpis_Insert_Input;
  on_conflict?: InputMaybe<Rozpis_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_SessionArgs = {
  objects: Array<Session_Insert_Input>;
  on_conflict?: InputMaybe<Session_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Session_OneArgs = {
  object: Session_Insert_Input;
  on_conflict?: InputMaybe<Session_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_SkupinyArgs = {
  objects: Array<Skupiny_Insert_Input>;
  on_conflict?: InputMaybe<Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Skupiny_OneArgs = {
  object: Skupiny_Insert_Input;
  on_conflict?: InputMaybe<Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_UpozorneniArgs = {
  objects: Array<Upozorneni_Insert_Input>;
  on_conflict?: InputMaybe<Upozorneni_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Upozorneni_OneArgs = {
  object: Upozorneni_Insert_Input;
  on_conflict?: InputMaybe<Upozorneni_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Upozorneni_SkupinyArgs = {
  objects: Array<Upozorneni_Skupiny_Insert_Input>;
  on_conflict?: InputMaybe<Upozorneni_Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Upozorneni_Skupiny_OneArgs = {
  object: Upozorneni_Skupiny_Insert_Input;
  on_conflict?: InputMaybe<Upozorneni_Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_UsersArgs = {
  objects: Array<Users_Insert_Input>;
  on_conflict?: InputMaybe<Users_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Users_OneArgs = {
  object: Users_Insert_Input;
  on_conflict?: InputMaybe<Users_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Users_SkupinyArgs = {
  objects: Array<Users_Skupiny_Insert_Input>;
  on_conflict?: InputMaybe<Users_Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Users_Skupiny_OneArgs = {
  object: Users_Skupiny_Insert_Input;
  on_conflict?: InputMaybe<Users_Skupiny_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_VideoArgs = {
  objects: Array<Video_Insert_Input>;
  on_conflict?: InputMaybe<Video_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Video_ListArgs = {
  objects: Array<Video_List_Insert_Input>;
  on_conflict?: InputMaybe<Video_List_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Video_List_OneArgs = {
  object: Video_List_Insert_Input;
  on_conflict?: InputMaybe<Video_List_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Video_OneArgs = {
  object: Video_Insert_Input;
  on_conflict?: InputMaybe<Video_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Video_SourceArgs = {
  objects: Array<Video_Source_Insert_Input>;
  on_conflict?: InputMaybe<Video_Source_On_Conflict>;
};


/** mutation root */
export type Mutation_RootInsert_Video_Source_OneArgs = {
  object: Video_Source_Insert_Input;
  on_conflict?: InputMaybe<Video_Source_On_Conflict>;
};


/** mutation root */
export type Mutation_RootUpdate_AkceArgs = {
  _inc?: InputMaybe<Akce_Inc_Input>;
  _set?: InputMaybe<Akce_Set_Input>;
  where: Akce_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Akce_By_PkArgs = {
  _inc?: InputMaybe<Akce_Inc_Input>;
  _set?: InputMaybe<Akce_Set_Input>;
  pk_columns: Akce_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Akce_ItemArgs = {
  _inc?: InputMaybe<Akce_Item_Inc_Input>;
  _set?: InputMaybe<Akce_Item_Set_Input>;
  where: Akce_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Akce_Item_By_PkArgs = {
  _inc?: InputMaybe<Akce_Item_Inc_Input>;
  _set?: InputMaybe<Akce_Item_Set_Input>;
  pk_columns: Akce_Item_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_AktualityArgs = {
  _inc?: InputMaybe<Aktuality_Inc_Input>;
  _set?: InputMaybe<Aktuality_Set_Input>;
  where: Aktuality_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Aktuality_AdminArgs = {
  _inc?: InputMaybe<Aktuality_Admin_Inc_Input>;
  _set?: InputMaybe<Aktuality_Admin_Set_Input>;
  where: Aktuality_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Aktuality_By_PkArgs = {
  _inc?: InputMaybe<Aktuality_Inc_Input>;
  _set?: InputMaybe<Aktuality_Set_Input>;
  pk_columns: Aktuality_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_DokumentyArgs = {
  _inc?: InputMaybe<Dokumenty_Inc_Input>;
  _set?: InputMaybe<Dokumenty_Set_Input>;
  where: Dokumenty_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Dokumenty_By_PkArgs = {
  _inc?: InputMaybe<Dokumenty_Inc_Input>;
  _set?: InputMaybe<Dokumenty_Set_Input>;
  pk_columns: Dokumenty_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Galerie_DirArgs = {
  _inc?: InputMaybe<Galerie_Dir_Inc_Input>;
  _set?: InputMaybe<Galerie_Dir_Set_Input>;
  where: Galerie_Dir_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Galerie_Dir_By_PkArgs = {
  _inc?: InputMaybe<Galerie_Dir_Inc_Input>;
  _set?: InputMaybe<Galerie_Dir_Set_Input>;
  pk_columns: Galerie_Dir_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Galerie_FotoArgs = {
  _inc?: InputMaybe<Galerie_Foto_Inc_Input>;
  _set?: InputMaybe<Galerie_Foto_Set_Input>;
  where: Galerie_Foto_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Galerie_Foto_By_PkArgs = {
  _inc?: InputMaybe<Galerie_Foto_Inc_Input>;
  _set?: InputMaybe<Galerie_Foto_Set_Input>;
  pk_columns: Galerie_Foto_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_NabidkaArgs = {
  _inc?: InputMaybe<Nabidka_Inc_Input>;
  _set?: InputMaybe<Nabidka_Set_Input>;
  where: Nabidka_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Nabidka_AdminArgs = {
  _inc?: InputMaybe<Nabidka_Admin_Inc_Input>;
  _set?: InputMaybe<Nabidka_Admin_Set_Input>;
  where: Nabidka_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Nabidka_By_PkArgs = {
  _inc?: InputMaybe<Nabidka_Inc_Input>;
  _set?: InputMaybe<Nabidka_Set_Input>;
  pk_columns: Nabidka_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Nabidka_ItemArgs = {
  _inc?: InputMaybe<Nabidka_Item_Inc_Input>;
  _set?: InputMaybe<Nabidka_Item_Set_Input>;
  where: Nabidka_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Nabidka_Item_By_PkArgs = {
  _inc?: InputMaybe<Nabidka_Item_Inc_Input>;
  _set?: InputMaybe<Nabidka_Item_Set_Input>;
  pk_columns: Nabidka_Item_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_ParametersArgs = {
  _set?: InputMaybe<Parameters_Set_Input>;
  where: Parameters_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Parameters_By_PkArgs = {
  _set?: InputMaybe<Parameters_Set_Input>;
  pk_columns: Parameters_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_ParyArgs = {
  _inc?: InputMaybe<Pary_Inc_Input>;
  _set?: InputMaybe<Pary_Set_Input>;
  where: Pary_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Pary_By_PkArgs = {
  _inc?: InputMaybe<Pary_Inc_Input>;
  _set?: InputMaybe<Pary_Set_Input>;
  pk_columns: Pary_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Pary_NavrhArgs = {
  _inc?: InputMaybe<Pary_Navrh_Inc_Input>;
  _set?: InputMaybe<Pary_Navrh_Set_Input>;
  where: Pary_Navrh_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Pary_Navrh_By_PkArgs = {
  _inc?: InputMaybe<Pary_Navrh_Inc_Input>;
  _set?: InputMaybe<Pary_Navrh_Set_Input>;
  pk_columns: Pary_Navrh_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_PermissionsArgs = {
  _inc?: InputMaybe<Permissions_Inc_Input>;
  _set?: InputMaybe<Permissions_Set_Input>;
  where: Permissions_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Permissions_By_PkArgs = {
  _inc?: InputMaybe<Permissions_Inc_Input>;
  _set?: InputMaybe<Permissions_Set_Input>;
  pk_columns: Permissions_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_CategoryArgs = {
  _inc?: InputMaybe<Platby_Category_Inc_Input>;
  _set?: InputMaybe<Platby_Category_Set_Input>;
  where: Platby_Category_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Category_By_PkArgs = {
  _inc?: InputMaybe<Platby_Category_Inc_Input>;
  _set?: InputMaybe<Platby_Category_Set_Input>;
  pk_columns: Platby_Category_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Category_GroupArgs = {
  _inc?: InputMaybe<Platby_Category_Group_Inc_Input>;
  _set?: InputMaybe<Platby_Category_Group_Set_Input>;
  where: Platby_Category_Group_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Category_Group_By_PkArgs = {
  _inc?: InputMaybe<Platby_Category_Group_Inc_Input>;
  _set?: InputMaybe<Platby_Category_Group_Set_Input>;
  pk_columns: Platby_Category_Group_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_GroupArgs = {
  _inc?: InputMaybe<Platby_Group_Inc_Input>;
  _set?: InputMaybe<Platby_Group_Set_Input>;
  where: Platby_Group_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Group_By_PkArgs = {
  _inc?: InputMaybe<Platby_Group_Inc_Input>;
  _set?: InputMaybe<Platby_Group_Set_Input>;
  pk_columns: Platby_Group_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Group_SkupinaArgs = {
  _inc?: InputMaybe<Platby_Group_Skupina_Inc_Input>;
  _set?: InputMaybe<Platby_Group_Skupina_Set_Input>;
  where: Platby_Group_Skupina_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Group_Skupina_By_PkArgs = {
  _inc?: InputMaybe<Platby_Group_Skupina_Inc_Input>;
  _set?: InputMaybe<Platby_Group_Skupina_Set_Input>;
  pk_columns: Platby_Group_Skupina_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_ItemArgs = {
  _inc?: InputMaybe<Platby_Item_Inc_Input>;
  _set?: InputMaybe<Platby_Item_Set_Input>;
  where: Platby_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Item_By_PkArgs = {
  _inc?: InputMaybe<Platby_Item_Inc_Input>;
  _set?: InputMaybe<Platby_Item_Set_Input>;
  pk_columns: Platby_Item_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_RawArgs = {
  _inc?: InputMaybe<Platby_Raw_Inc_Input>;
  _set?: InputMaybe<Platby_Raw_Set_Input>;
  where: Platby_Raw_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Platby_Raw_By_PkArgs = {
  _inc?: InputMaybe<Platby_Raw_Inc_Input>;
  _set?: InputMaybe<Platby_Raw_Set_Input>;
  pk_columns: Platby_Raw_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_RozpisArgs = {
  _inc?: InputMaybe<Rozpis_Inc_Input>;
  _set?: InputMaybe<Rozpis_Set_Input>;
  where: Rozpis_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Rozpis_AdminArgs = {
  _inc?: InputMaybe<Rozpis_Admin_Inc_Input>;
  _set?: InputMaybe<Rozpis_Admin_Set_Input>;
  where: Rozpis_Admin_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Rozpis_By_PkArgs = {
  _inc?: InputMaybe<Rozpis_Inc_Input>;
  _set?: InputMaybe<Rozpis_Set_Input>;
  pk_columns: Rozpis_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Rozpis_ItemArgs = {
  _inc?: InputMaybe<Rozpis_Item_Inc_Input>;
  _set?: InputMaybe<Rozpis_Item_Set_Input>;
  where: Rozpis_Item_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Rozpis_Item_By_PkArgs = {
  _inc?: InputMaybe<Rozpis_Item_Inc_Input>;
  _set?: InputMaybe<Rozpis_Item_Set_Input>;
  pk_columns: Rozpis_Item_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_SessionArgs = {
  _inc?: InputMaybe<Session_Inc_Input>;
  _set?: InputMaybe<Session_Set_Input>;
  where: Session_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Session_By_PkArgs = {
  _inc?: InputMaybe<Session_Inc_Input>;
  _set?: InputMaybe<Session_Set_Input>;
  pk_columns: Session_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_SkupinyArgs = {
  _inc?: InputMaybe<Skupiny_Inc_Input>;
  _set?: InputMaybe<Skupiny_Set_Input>;
  where: Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Skupiny_By_PkArgs = {
  _inc?: InputMaybe<Skupiny_Inc_Input>;
  _set?: InputMaybe<Skupiny_Set_Input>;
  pk_columns: Skupiny_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_UpozorneniArgs = {
  _inc?: InputMaybe<Upozorneni_Inc_Input>;
  _set?: InputMaybe<Upozorneni_Set_Input>;
  where: Upozorneni_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Upozorneni_By_PkArgs = {
  _inc?: InputMaybe<Upozorneni_Inc_Input>;
  _set?: InputMaybe<Upozorneni_Set_Input>;
  pk_columns: Upozorneni_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Upozorneni_SkupinyArgs = {
  _inc?: InputMaybe<Upozorneni_Skupiny_Inc_Input>;
  _set?: InputMaybe<Upozorneni_Skupiny_Set_Input>;
  where: Upozorneni_Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Upozorneni_Skupiny_By_PkArgs = {
  _inc?: InputMaybe<Upozorneni_Skupiny_Inc_Input>;
  _set?: InputMaybe<Upozorneni_Skupiny_Set_Input>;
  pk_columns: Upozorneni_Skupiny_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_UsersArgs = {
  _inc?: InputMaybe<Users_Inc_Input>;
  _set?: InputMaybe<Users_Set_Input>;
  where: Users_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Users_By_PkArgs = {
  _inc?: InputMaybe<Users_Inc_Input>;
  _set?: InputMaybe<Users_Set_Input>;
  pk_columns: Users_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Users_SkupinyArgs = {
  _inc?: InputMaybe<Users_Skupiny_Inc_Input>;
  _set?: InputMaybe<Users_Skupiny_Set_Input>;
  where: Users_Skupiny_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Users_Skupiny_By_PkArgs = {
  _inc?: InputMaybe<Users_Skupiny_Inc_Input>;
  _set?: InputMaybe<Users_Skupiny_Set_Input>;
  pk_columns: Users_Skupiny_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_VideoArgs = {
  _inc?: InputMaybe<Video_Inc_Input>;
  _set?: InputMaybe<Video_Set_Input>;
  where: Video_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Video_By_PkArgs = {
  _inc?: InputMaybe<Video_Inc_Input>;
  _set?: InputMaybe<Video_Set_Input>;
  pk_columns: Video_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Video_ListArgs = {
  _inc?: InputMaybe<Video_List_Inc_Input>;
  _set?: InputMaybe<Video_List_Set_Input>;
  where: Video_List_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Video_List_By_PkArgs = {
  _inc?: InputMaybe<Video_List_Inc_Input>;
  _set?: InputMaybe<Video_List_Set_Input>;
  pk_columns: Video_List_Pk_Columns_Input;
};


/** mutation root */
export type Mutation_RootUpdate_Video_SourceArgs = {
  _inc?: InputMaybe<Video_Source_Inc_Input>;
  _set?: InputMaybe<Video_Source_Set_Input>;
  where: Video_Source_Bool_Exp;
};


/** mutation root */
export type Mutation_RootUpdate_Video_Source_By_PkArgs = {
  _inc?: InputMaybe<Video_Source_Inc_Input>;
  _set?: InputMaybe<Video_Source_Set_Input>;
  pk_columns: Video_Source_Pk_Columns_Input;
};

/** columns and relationships of "nabidka" */
export type Nabidka = {
  __typename?: 'nabidka';
  n_do: Scalars['date'];
  n_id: Scalars['bigint'];
  n_lock: Scalars['Boolean'];
  n_max_pocet_hod: Scalars['bigint'];
  n_od: Scalars['date'];
  n_pocet_hod: Scalars['smallint'];
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener: Scalars['bigint'];
  n_visible: Scalars['Boolean'];
  /** An array relationship */
  nabidka_items: Array<Nabidka_Item>;
  /** An aggregate relationship */
  nabidka_items_aggregate: Nabidka_Item_Aggregate;
  /** An object relationship */
  user: Users;
};


/** columns and relationships of "nabidka" */
export type NabidkaNabidka_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


/** columns and relationships of "nabidka" */
export type NabidkaNabidka_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};

/** columns and relationships of "nabidka_admin" */
export type Nabidka_Admin = {
  __typename?: 'nabidka_admin';
  n_do?: Maybe<Scalars['date']>;
  n_id?: Maybe<Scalars['bigint']>;
  n_lock?: Maybe<Scalars['Boolean']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_od?: Maybe<Scalars['date']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener?: Maybe<Scalars['bigint']>;
  n_visible?: Maybe<Scalars['Boolean']>;
  /** An array relationship */
  nabidka_items: Array<Nabidka_Item>;
  /** An aggregate relationship */
  nabidka_items_aggregate: Nabidka_Item_Aggregate;
  /** An object relationship */
  user?: Maybe<Users>;
};


/** columns and relationships of "nabidka_admin" */
export type Nabidka_AdminNabidka_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


/** columns and relationships of "nabidka_admin" */
export type Nabidka_AdminNabidka_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};

/** aggregated selection of "nabidka_admin" */
export type Nabidka_Admin_Aggregate = {
  __typename?: 'nabidka_admin_aggregate';
  aggregate?: Maybe<Nabidka_Admin_Aggregate_Fields>;
  nodes: Array<Nabidka_Admin>;
};

/** aggregate fields of "nabidka_admin" */
export type Nabidka_Admin_Aggregate_Fields = {
  __typename?: 'nabidka_admin_aggregate_fields';
  avg?: Maybe<Nabidka_Admin_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Nabidka_Admin_Max_Fields>;
  min?: Maybe<Nabidka_Admin_Min_Fields>;
  stddev?: Maybe<Nabidka_Admin_Stddev_Fields>;
  stddev_pop?: Maybe<Nabidka_Admin_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Nabidka_Admin_Stddev_Samp_Fields>;
  sum?: Maybe<Nabidka_Admin_Sum_Fields>;
  var_pop?: Maybe<Nabidka_Admin_Var_Pop_Fields>;
  var_samp?: Maybe<Nabidka_Admin_Var_Samp_Fields>;
  variance?: Maybe<Nabidka_Admin_Variance_Fields>;
};


/** aggregate fields of "nabidka_admin" */
export type Nabidka_Admin_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Nabidka_Admin_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Nabidka_Admin_Avg_Fields = {
  __typename?: 'nabidka_admin_avg_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "nabidka_admin". All fields are combined with a logical 'AND'. */
export type Nabidka_Admin_Bool_Exp = {
  _and?: InputMaybe<Array<Nabidka_Admin_Bool_Exp>>;
  _not?: InputMaybe<Nabidka_Admin_Bool_Exp>;
  _or?: InputMaybe<Array<Nabidka_Admin_Bool_Exp>>;
  n_do?: InputMaybe<Date_Comparison_Exp>;
  n_id?: InputMaybe<Bigint_Comparison_Exp>;
  n_lock?: InputMaybe<Boolean_Comparison_Exp>;
  n_max_pocet_hod?: InputMaybe<Bigint_Comparison_Exp>;
  n_od?: InputMaybe<Date_Comparison_Exp>;
  n_pocet_hod?: InputMaybe<Smallint_Comparison_Exp>;
  n_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  n_trener?: InputMaybe<Bigint_Comparison_Exp>;
  n_visible?: InputMaybe<Boolean_Comparison_Exp>;
  nabidka_items?: InputMaybe<Nabidka_Item_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** input type for incrementing numeric columns in table "nabidka_admin" */
export type Nabidka_Admin_Inc_Input = {
  n_id?: InputMaybe<Scalars['bigint']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "nabidka_admin" */
export type Nabidka_Admin_Insert_Input = {
  n_do?: InputMaybe<Scalars['date']>;
  n_id?: InputMaybe<Scalars['bigint']>;
  n_lock?: InputMaybe<Scalars['Boolean']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_od?: InputMaybe<Scalars['date']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_timestamp?: InputMaybe<Scalars['timestamptz']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
  n_visible?: InputMaybe<Scalars['Boolean']>;
  nabidka_items?: InputMaybe<Nabidka_Item_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Nabidka_Admin_Max_Fields = {
  __typename?: 'nabidka_admin_max_fields';
  n_do?: Maybe<Scalars['date']>;
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_od?: Maybe<Scalars['date']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** aggregate min on columns */
export type Nabidka_Admin_Min_Fields = {
  __typename?: 'nabidka_admin_min_fields';
  n_do?: Maybe<Scalars['date']>;
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_od?: Maybe<Scalars['date']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** response of any mutation on the table "nabidka_admin" */
export type Nabidka_Admin_Mutation_Response = {
  __typename?: 'nabidka_admin_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Nabidka_Admin>;
};

/** Ordering options when selecting data from "nabidka_admin". */
export type Nabidka_Admin_Order_By = {
  n_do?: InputMaybe<Order_By>;
  n_id?: InputMaybe<Order_By>;
  n_lock?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_od?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_timestamp?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
  n_visible?: InputMaybe<Order_By>;
  nabidka_items_aggregate?: InputMaybe<Nabidka_Item_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** select columns of table "nabidka_admin" */
export enum Nabidka_Admin_Select_Column {
  /** column name */
  NDo = 'n_do',
  /** column name */
  NId = 'n_id',
  /** column name */
  NLock = 'n_lock',
  /** column name */
  NMaxPocetHod = 'n_max_pocet_hod',
  /** column name */
  NOd = 'n_od',
  /** column name */
  NPocetHod = 'n_pocet_hod',
  /** column name */
  NTimestamp = 'n_timestamp',
  /** column name */
  NTrener = 'n_trener',
  /** column name */
  NVisible = 'n_visible'
}

/** input type for updating data in table "nabidka_admin" */
export type Nabidka_Admin_Set_Input = {
  n_do?: InputMaybe<Scalars['date']>;
  n_id?: InputMaybe<Scalars['bigint']>;
  n_lock?: InputMaybe<Scalars['Boolean']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_od?: InputMaybe<Scalars['date']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_timestamp?: InputMaybe<Scalars['timestamptz']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
  n_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Nabidka_Admin_Stddev_Fields = {
  __typename?: 'nabidka_admin_stddev_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Nabidka_Admin_Stddev_Pop_Fields = {
  __typename?: 'nabidka_admin_stddev_pop_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Nabidka_Admin_Stddev_Samp_Fields = {
  __typename?: 'nabidka_admin_stddev_samp_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Nabidka_Admin_Sum_Fields = {
  __typename?: 'nabidka_admin_sum_fields';
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** aggregate var_pop on columns */
export type Nabidka_Admin_Var_Pop_Fields = {
  __typename?: 'nabidka_admin_var_pop_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Nabidka_Admin_Var_Samp_Fields = {
  __typename?: 'nabidka_admin_var_samp_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Nabidka_Admin_Variance_Fields = {
  __typename?: 'nabidka_admin_variance_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** aggregated selection of "nabidka" */
export type Nabidka_Aggregate = {
  __typename?: 'nabidka_aggregate';
  aggregate?: Maybe<Nabidka_Aggregate_Fields>;
  nodes: Array<Nabidka>;
};

/** aggregate fields of "nabidka" */
export type Nabidka_Aggregate_Fields = {
  __typename?: 'nabidka_aggregate_fields';
  avg?: Maybe<Nabidka_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Nabidka_Max_Fields>;
  min?: Maybe<Nabidka_Min_Fields>;
  stddev?: Maybe<Nabidka_Stddev_Fields>;
  stddev_pop?: Maybe<Nabidka_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Nabidka_Stddev_Samp_Fields>;
  sum?: Maybe<Nabidka_Sum_Fields>;
  var_pop?: Maybe<Nabidka_Var_Pop_Fields>;
  var_samp?: Maybe<Nabidka_Var_Samp_Fields>;
  variance?: Maybe<Nabidka_Variance_Fields>;
};


/** aggregate fields of "nabidka" */
export type Nabidka_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Nabidka_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "nabidka" */
export type Nabidka_Aggregate_Order_By = {
  avg?: InputMaybe<Nabidka_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Nabidka_Max_Order_By>;
  min?: InputMaybe<Nabidka_Min_Order_By>;
  stddev?: InputMaybe<Nabidka_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Nabidka_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Nabidka_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Nabidka_Sum_Order_By>;
  var_pop?: InputMaybe<Nabidka_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Nabidka_Var_Samp_Order_By>;
  variance?: InputMaybe<Nabidka_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "nabidka" */
export type Nabidka_Arr_Rel_Insert_Input = {
  data: Array<Nabidka_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Nabidka_On_Conflict>;
};

/** aggregate avg on columns */
export type Nabidka_Avg_Fields = {
  __typename?: 'nabidka_avg_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "nabidka" */
export type Nabidka_Avg_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "nabidka". All fields are combined with a logical 'AND'. */
export type Nabidka_Bool_Exp = {
  _and?: InputMaybe<Array<Nabidka_Bool_Exp>>;
  _not?: InputMaybe<Nabidka_Bool_Exp>;
  _or?: InputMaybe<Array<Nabidka_Bool_Exp>>;
  n_do?: InputMaybe<Date_Comparison_Exp>;
  n_id?: InputMaybe<Bigint_Comparison_Exp>;
  n_lock?: InputMaybe<Boolean_Comparison_Exp>;
  n_max_pocet_hod?: InputMaybe<Bigint_Comparison_Exp>;
  n_od?: InputMaybe<Date_Comparison_Exp>;
  n_pocet_hod?: InputMaybe<Smallint_Comparison_Exp>;
  n_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  n_trener?: InputMaybe<Bigint_Comparison_Exp>;
  n_visible?: InputMaybe<Boolean_Comparison_Exp>;
  nabidka_items?: InputMaybe<Nabidka_Item_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "nabidka" */
export enum Nabidka_Constraint {
  /** unique or primary key constraint */
  Idx_24622Primary = 'idx_24622_primary'
}

/** input type for incrementing numeric columns in table "nabidka" */
export type Nabidka_Inc_Input = {
  n_id?: InputMaybe<Scalars['bigint']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "nabidka" */
export type Nabidka_Insert_Input = {
  n_do?: InputMaybe<Scalars['date']>;
  n_id?: InputMaybe<Scalars['bigint']>;
  n_lock?: InputMaybe<Scalars['Boolean']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_od?: InputMaybe<Scalars['date']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_timestamp?: InputMaybe<Scalars['timestamptz']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
  n_visible?: InputMaybe<Scalars['Boolean']>;
  nabidka_items?: InputMaybe<Nabidka_Item_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** columns and relationships of "nabidka_item" */
export type Nabidka_Item = {
  __typename?: 'nabidka_item';
  /** An object relationship */
  nabidka: Nabidka;
  ni_id: Scalars['bigint'];
  ni_id_rodic: Scalars['bigint'];
  ni_lock: Scalars['Boolean'];
  ni_partner: Scalars['bigint'];
  ni_pocet_hod: Scalars['smallint'];
  /** An object relationship */
  pary: Pary;
};

/** aggregated selection of "nabidka_item" */
export type Nabidka_Item_Aggregate = {
  __typename?: 'nabidka_item_aggregate';
  aggregate?: Maybe<Nabidka_Item_Aggregate_Fields>;
  nodes: Array<Nabidka_Item>;
};

/** aggregate fields of "nabidka_item" */
export type Nabidka_Item_Aggregate_Fields = {
  __typename?: 'nabidka_item_aggregate_fields';
  avg?: Maybe<Nabidka_Item_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Nabidka_Item_Max_Fields>;
  min?: Maybe<Nabidka_Item_Min_Fields>;
  stddev?: Maybe<Nabidka_Item_Stddev_Fields>;
  stddev_pop?: Maybe<Nabidka_Item_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Nabidka_Item_Stddev_Samp_Fields>;
  sum?: Maybe<Nabidka_Item_Sum_Fields>;
  var_pop?: Maybe<Nabidka_Item_Var_Pop_Fields>;
  var_samp?: Maybe<Nabidka_Item_Var_Samp_Fields>;
  variance?: Maybe<Nabidka_Item_Variance_Fields>;
};


/** aggregate fields of "nabidka_item" */
export type Nabidka_Item_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "nabidka_item" */
export type Nabidka_Item_Aggregate_Order_By = {
  avg?: InputMaybe<Nabidka_Item_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Nabidka_Item_Max_Order_By>;
  min?: InputMaybe<Nabidka_Item_Min_Order_By>;
  stddev?: InputMaybe<Nabidka_Item_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Nabidka_Item_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Nabidka_Item_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Nabidka_Item_Sum_Order_By>;
  var_pop?: InputMaybe<Nabidka_Item_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Nabidka_Item_Var_Samp_Order_By>;
  variance?: InputMaybe<Nabidka_Item_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "nabidka_item" */
export type Nabidka_Item_Arr_Rel_Insert_Input = {
  data: Array<Nabidka_Item_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Nabidka_Item_On_Conflict>;
};

/** aggregate avg on columns */
export type Nabidka_Item_Avg_Fields = {
  __typename?: 'nabidka_item_avg_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "nabidka_item" */
export type Nabidka_Item_Avg_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "nabidka_item". All fields are combined with a logical 'AND'. */
export type Nabidka_Item_Bool_Exp = {
  _and?: InputMaybe<Array<Nabidka_Item_Bool_Exp>>;
  _not?: InputMaybe<Nabidka_Item_Bool_Exp>;
  _or?: InputMaybe<Array<Nabidka_Item_Bool_Exp>>;
  nabidka?: InputMaybe<Nabidka_Bool_Exp>;
  ni_id?: InputMaybe<Bigint_Comparison_Exp>;
  ni_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  ni_lock?: InputMaybe<Boolean_Comparison_Exp>;
  ni_partner?: InputMaybe<Bigint_Comparison_Exp>;
  ni_pocet_hod?: InputMaybe<Smallint_Comparison_Exp>;
  pary?: InputMaybe<Pary_Bool_Exp>;
};

/** unique or primary key constraints on table "nabidka_item" */
export enum Nabidka_Item_Constraint {
  /** unique or primary key constraint */
  Idx_24632NiIdRodic = 'idx_24632_ni_id_rodic',
  /** unique or primary key constraint */
  Idx_24632Primary = 'idx_24632_primary'
}

/** input type for incrementing numeric columns in table "nabidka_item" */
export type Nabidka_Item_Inc_Input = {
  ni_id?: InputMaybe<Scalars['bigint']>;
  ni_id_rodic?: InputMaybe<Scalars['bigint']>;
  ni_partner?: InputMaybe<Scalars['bigint']>;
  ni_pocet_hod?: InputMaybe<Scalars['smallint']>;
};

/** input type for inserting data into table "nabidka_item" */
export type Nabidka_Item_Insert_Input = {
  nabidka?: InputMaybe<Nabidka_Obj_Rel_Insert_Input>;
  ni_id?: InputMaybe<Scalars['bigint']>;
  ni_id_rodic?: InputMaybe<Scalars['bigint']>;
  ni_lock?: InputMaybe<Scalars['Boolean']>;
  ni_partner?: InputMaybe<Scalars['bigint']>;
  ni_pocet_hod?: InputMaybe<Scalars['smallint']>;
  pary?: InputMaybe<Pary_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Nabidka_Item_Max_Fields = {
  __typename?: 'nabidka_item_max_fields';
  ni_id?: Maybe<Scalars['bigint']>;
  ni_id_rodic?: Maybe<Scalars['bigint']>;
  ni_partner?: Maybe<Scalars['bigint']>;
  ni_pocet_hod?: Maybe<Scalars['smallint']>;
};

/** order by max() on columns of table "nabidka_item" */
export type Nabidka_Item_Max_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Nabidka_Item_Min_Fields = {
  __typename?: 'nabidka_item_min_fields';
  ni_id?: Maybe<Scalars['bigint']>;
  ni_id_rodic?: Maybe<Scalars['bigint']>;
  ni_partner?: Maybe<Scalars['bigint']>;
  ni_pocet_hod?: Maybe<Scalars['smallint']>;
};

/** order by min() on columns of table "nabidka_item" */
export type Nabidka_Item_Min_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "nabidka_item" */
export type Nabidka_Item_Mutation_Response = {
  __typename?: 'nabidka_item_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Nabidka_Item>;
};

/** on conflict condition type for table "nabidka_item" */
export type Nabidka_Item_On_Conflict = {
  constraint: Nabidka_Item_Constraint;
  update_columns?: Array<Nabidka_Item_Update_Column>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};

/** Ordering options when selecting data from "nabidka_item". */
export type Nabidka_Item_Order_By = {
  nabidka?: InputMaybe<Nabidka_Order_By>;
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_lock?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
  pary?: InputMaybe<Pary_Order_By>;
};

/** primary key columns input for table: nabidka_item */
export type Nabidka_Item_Pk_Columns_Input = {
  ni_id: Scalars['bigint'];
};

/** select columns of table "nabidka_item" */
export enum Nabidka_Item_Select_Column {
  /** column name */
  NiId = 'ni_id',
  /** column name */
  NiIdRodic = 'ni_id_rodic',
  /** column name */
  NiLock = 'ni_lock',
  /** column name */
  NiPartner = 'ni_partner',
  /** column name */
  NiPocetHod = 'ni_pocet_hod'
}

/** input type for updating data in table "nabidka_item" */
export type Nabidka_Item_Set_Input = {
  ni_id?: InputMaybe<Scalars['bigint']>;
  ni_id_rodic?: InputMaybe<Scalars['bigint']>;
  ni_lock?: InputMaybe<Scalars['Boolean']>;
  ni_partner?: InputMaybe<Scalars['bigint']>;
  ni_pocet_hod?: InputMaybe<Scalars['smallint']>;
};

/** aggregate stddev on columns */
export type Nabidka_Item_Stddev_Fields = {
  __typename?: 'nabidka_item_stddev_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "nabidka_item" */
export type Nabidka_Item_Stddev_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Nabidka_Item_Stddev_Pop_Fields = {
  __typename?: 'nabidka_item_stddev_pop_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "nabidka_item" */
export type Nabidka_Item_Stddev_Pop_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Nabidka_Item_Stddev_Samp_Fields = {
  __typename?: 'nabidka_item_stddev_samp_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "nabidka_item" */
export type Nabidka_Item_Stddev_Samp_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Nabidka_Item_Sum_Fields = {
  __typename?: 'nabidka_item_sum_fields';
  ni_id?: Maybe<Scalars['bigint']>;
  ni_id_rodic?: Maybe<Scalars['bigint']>;
  ni_partner?: Maybe<Scalars['bigint']>;
  ni_pocet_hod?: Maybe<Scalars['smallint']>;
};

/** order by sum() on columns of table "nabidka_item" */
export type Nabidka_Item_Sum_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** update columns of table "nabidka_item" */
export enum Nabidka_Item_Update_Column {
  /** column name */
  NiId = 'ni_id',
  /** column name */
  NiIdRodic = 'ni_id_rodic',
  /** column name */
  NiLock = 'ni_lock',
  /** column name */
  NiPartner = 'ni_partner',
  /** column name */
  NiPocetHod = 'ni_pocet_hod'
}

/** aggregate var_pop on columns */
export type Nabidka_Item_Var_Pop_Fields = {
  __typename?: 'nabidka_item_var_pop_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "nabidka_item" */
export type Nabidka_Item_Var_Pop_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Nabidka_Item_Var_Samp_Fields = {
  __typename?: 'nabidka_item_var_samp_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "nabidka_item" */
export type Nabidka_Item_Var_Samp_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Nabidka_Item_Variance_Fields = {
  __typename?: 'nabidka_item_variance_fields';
  ni_id?: Maybe<Scalars['Float']>;
  ni_id_rodic?: Maybe<Scalars['Float']>;
  ni_partner?: Maybe<Scalars['Float']>;
  ni_pocet_hod?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "nabidka_item" */
export type Nabidka_Item_Variance_Order_By = {
  ni_id?: InputMaybe<Order_By>;
  ni_id_rodic?: InputMaybe<Order_By>;
  ni_partner?: InputMaybe<Order_By>;
  ni_pocet_hod?: InputMaybe<Order_By>;
};

/** aggregate max on columns */
export type Nabidka_Max_Fields = {
  __typename?: 'nabidka_max_fields';
  n_do?: Maybe<Scalars['date']>;
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_od?: Maybe<Scalars['date']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "nabidka" */
export type Nabidka_Max_Order_By = {
  n_do?: InputMaybe<Order_By>;
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_od?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_timestamp?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Nabidka_Min_Fields = {
  __typename?: 'nabidka_min_fields';
  n_do?: Maybe<Scalars['date']>;
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_od?: Maybe<Scalars['date']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_timestamp?: Maybe<Scalars['timestamptz']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "nabidka" */
export type Nabidka_Min_Order_By = {
  n_do?: InputMaybe<Order_By>;
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_od?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_timestamp?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "nabidka" */
export type Nabidka_Mutation_Response = {
  __typename?: 'nabidka_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Nabidka>;
};

/** input type for inserting object relation for remote table "nabidka" */
export type Nabidka_Obj_Rel_Insert_Input = {
  data: Nabidka_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Nabidka_On_Conflict>;
};

/** on conflict condition type for table "nabidka" */
export type Nabidka_On_Conflict = {
  constraint: Nabidka_Constraint;
  update_columns?: Array<Nabidka_Update_Column>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};

/** Ordering options when selecting data from "nabidka". */
export type Nabidka_Order_By = {
  n_do?: InputMaybe<Order_By>;
  n_id?: InputMaybe<Order_By>;
  n_lock?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_od?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_timestamp?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
  n_visible?: InputMaybe<Order_By>;
  nabidka_items_aggregate?: InputMaybe<Nabidka_Item_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: nabidka */
export type Nabidka_Pk_Columns_Input = {
  n_id: Scalars['bigint'];
};

/** select columns of table "nabidka" */
export enum Nabidka_Select_Column {
  /** column name */
  NDo = 'n_do',
  /** column name */
  NId = 'n_id',
  /** column name */
  NLock = 'n_lock',
  /** column name */
  NMaxPocetHod = 'n_max_pocet_hod',
  /** column name */
  NOd = 'n_od',
  /** column name */
  NPocetHod = 'n_pocet_hod',
  /** column name */
  NTimestamp = 'n_timestamp',
  /** column name */
  NTrener = 'n_trener',
  /** column name */
  NVisible = 'n_visible'
}

/** input type for updating data in table "nabidka" */
export type Nabidka_Set_Input = {
  n_do?: InputMaybe<Scalars['date']>;
  n_id?: InputMaybe<Scalars['bigint']>;
  n_lock?: InputMaybe<Scalars['Boolean']>;
  n_max_pocet_hod?: InputMaybe<Scalars['bigint']>;
  n_od?: InputMaybe<Scalars['date']>;
  n_pocet_hod?: InputMaybe<Scalars['smallint']>;
  n_timestamp?: InputMaybe<Scalars['timestamptz']>;
  n_trener?: InputMaybe<Scalars['bigint']>;
  n_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Nabidka_Stddev_Fields = {
  __typename?: 'nabidka_stddev_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "nabidka" */
export type Nabidka_Stddev_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Nabidka_Stddev_Pop_Fields = {
  __typename?: 'nabidka_stddev_pop_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "nabidka" */
export type Nabidka_Stddev_Pop_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Nabidka_Stddev_Samp_Fields = {
  __typename?: 'nabidka_stddev_samp_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "nabidka" */
export type Nabidka_Stddev_Samp_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Nabidka_Sum_Fields = {
  __typename?: 'nabidka_sum_fields';
  n_id?: Maybe<Scalars['bigint']>;
  n_max_pocet_hod?: Maybe<Scalars['bigint']>;
  n_pocet_hod?: Maybe<Scalars['smallint']>;
  n_trener?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "nabidka" */
export type Nabidka_Sum_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** update columns of table "nabidka" */
export enum Nabidka_Update_Column {
  /** column name */
  NDo = 'n_do',
  /** column name */
  NId = 'n_id',
  /** column name */
  NLock = 'n_lock',
  /** column name */
  NMaxPocetHod = 'n_max_pocet_hod',
  /** column name */
  NOd = 'n_od',
  /** column name */
  NPocetHod = 'n_pocet_hod',
  /** column name */
  NTimestamp = 'n_timestamp',
  /** column name */
  NTrener = 'n_trener',
  /** column name */
  NVisible = 'n_visible'
}

/** aggregate var_pop on columns */
export type Nabidka_Var_Pop_Fields = {
  __typename?: 'nabidka_var_pop_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "nabidka" */
export type Nabidka_Var_Pop_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Nabidka_Var_Samp_Fields = {
  __typename?: 'nabidka_var_samp_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "nabidka" */
export type Nabidka_Var_Samp_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Nabidka_Variance_Fields = {
  __typename?: 'nabidka_variance_fields';
  n_id?: Maybe<Scalars['Float']>;
  n_max_pocet_hod?: Maybe<Scalars['Float']>;
  n_pocet_hod?: Maybe<Scalars['Float']>;
  n_trener?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "nabidka" */
export type Nabidka_Variance_Order_By = {
  n_id?: InputMaybe<Order_By>;
  n_max_pocet_hod?: InputMaybe<Order_By>;
  n_pocet_hod?: InputMaybe<Order_By>;
  n_trener?: InputMaybe<Order_By>;
};

/** Boolean expression to compare columns of type "numeric". All fields are combined with logical 'AND'. */
export type Numeric_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['numeric']>;
  _gt?: InputMaybe<Scalars['numeric']>;
  _gte?: InputMaybe<Scalars['numeric']>;
  _in?: InputMaybe<Array<Scalars['numeric']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['numeric']>;
  _lte?: InputMaybe<Scalars['numeric']>;
  _neq?: InputMaybe<Scalars['numeric']>;
  _nin?: InputMaybe<Array<Scalars['numeric']>>;
};

/** column ordering options */
export enum Order_By {
  /** in ascending order, nulls last */
  Asc = 'asc',
  /** in ascending order, nulls first */
  AscNullsFirst = 'asc_nulls_first',
  /** in ascending order, nulls last */
  AscNullsLast = 'asc_nulls_last',
  /** in descending order, nulls first */
  Desc = 'desc',
  /** in descending order, nulls first */
  DescNullsFirst = 'desc_nulls_first',
  /** in descending order, nulls last */
  DescNullsLast = 'desc_nulls_last'
}

/** columns and relationships of "parameters" */
export type Parameters = {
  __typename?: 'parameters';
  pa_name: Scalars['String'];
  pa_value: Scalars['String'];
};

/** aggregated selection of "parameters" */
export type Parameters_Aggregate = {
  __typename?: 'parameters_aggregate';
  aggregate?: Maybe<Parameters_Aggregate_Fields>;
  nodes: Array<Parameters>;
};

/** aggregate fields of "parameters" */
export type Parameters_Aggregate_Fields = {
  __typename?: 'parameters_aggregate_fields';
  count: Scalars['Int'];
  max?: Maybe<Parameters_Max_Fields>;
  min?: Maybe<Parameters_Min_Fields>;
};


/** aggregate fields of "parameters" */
export type Parameters_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Parameters_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** Boolean expression to filter rows from the table "parameters". All fields are combined with a logical 'AND'. */
export type Parameters_Bool_Exp = {
  _and?: InputMaybe<Array<Parameters_Bool_Exp>>;
  _not?: InputMaybe<Parameters_Bool_Exp>;
  _or?: InputMaybe<Array<Parameters_Bool_Exp>>;
  pa_name?: InputMaybe<String_Comparison_Exp>;
  pa_value?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "parameters" */
export enum Parameters_Constraint {
  /** unique or primary key constraint */
  Idx_24638Primary = 'idx_24638_primary'
}

/** input type for inserting data into table "parameters" */
export type Parameters_Insert_Input = {
  pa_name?: InputMaybe<Scalars['String']>;
  pa_value?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Parameters_Max_Fields = {
  __typename?: 'parameters_max_fields';
  pa_name?: Maybe<Scalars['String']>;
  pa_value?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Parameters_Min_Fields = {
  __typename?: 'parameters_min_fields';
  pa_name?: Maybe<Scalars['String']>;
  pa_value?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "parameters" */
export type Parameters_Mutation_Response = {
  __typename?: 'parameters_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Parameters>;
};

/** on conflict condition type for table "parameters" */
export type Parameters_On_Conflict = {
  constraint: Parameters_Constraint;
  update_columns?: Array<Parameters_Update_Column>;
  where?: InputMaybe<Parameters_Bool_Exp>;
};

/** Ordering options when selecting data from "parameters". */
export type Parameters_Order_By = {
  pa_name?: InputMaybe<Order_By>;
  pa_value?: InputMaybe<Order_By>;
};

/** primary key columns input for table: parameters */
export type Parameters_Pk_Columns_Input = {
  pa_name: Scalars['String'];
};

/** select columns of table "parameters" */
export enum Parameters_Select_Column {
  /** column name */
  PaName = 'pa_name',
  /** column name */
  PaValue = 'pa_value'
}

/** input type for updating data in table "parameters" */
export type Parameters_Set_Input = {
  pa_name?: InputMaybe<Scalars['String']>;
  pa_value?: InputMaybe<Scalars['String']>;
};

/** update columns of table "parameters" */
export enum Parameters_Update_Column {
  /** column name */
  PaName = 'pa_name',
  /** column name */
  PaValue = 'pa_value'
}

/** columns and relationships of "pary" */
export type Pary = {
  __typename?: 'pary';
  /** An array relationship */
  nabidka_items: Array<Nabidka_Item>;
  /** An aggregate relationship */
  nabidka_items_aggregate: Nabidka_Item_Aggregate;
  p_archiv: Scalars['Boolean'];
  p_hodnoceni: Scalars['Int'];
  p_id: Scalars['bigint'];
  p_id_partner: Scalars['bigint'];
  p_id_partnerka?: Maybe<Scalars['bigint']>;
  p_lat_body: Scalars['Int'];
  p_lat_finale: Scalars['Boolean'];
  p_lat_trida: Scalars['pary_p_lat_trida'];
  p_stt_body: Scalars['Int'];
  p_stt_finale: Scalars['Boolean'];
  p_stt_trida: Scalars['pary_p_stt_trida'];
  p_timestamp_add: Scalars['timestamptz'];
  p_timestamp_archive?: Maybe<Scalars['timestamptz']>;
  /** An array relationship */
  rozpis_items: Array<Rozpis_Item>;
  /** An aggregate relationship */
  rozpis_items_aggregate: Rozpis_Item_Aggregate;
  /** An object relationship */
  user: Users;
};


/** columns and relationships of "pary" */
export type ParyNabidka_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


/** columns and relationships of "pary" */
export type ParyNabidka_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


/** columns and relationships of "pary" */
export type ParyRozpis_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


/** columns and relationships of "pary" */
export type ParyRozpis_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};

/** aggregated selection of "pary" */
export type Pary_Aggregate = {
  __typename?: 'pary_aggregate';
  aggregate?: Maybe<Pary_Aggregate_Fields>;
  nodes: Array<Pary>;
};

/** aggregate fields of "pary" */
export type Pary_Aggregate_Fields = {
  __typename?: 'pary_aggregate_fields';
  avg?: Maybe<Pary_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Pary_Max_Fields>;
  min?: Maybe<Pary_Min_Fields>;
  stddev?: Maybe<Pary_Stddev_Fields>;
  stddev_pop?: Maybe<Pary_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Pary_Stddev_Samp_Fields>;
  sum?: Maybe<Pary_Sum_Fields>;
  var_pop?: Maybe<Pary_Var_Pop_Fields>;
  var_samp?: Maybe<Pary_Var_Samp_Fields>;
  variance?: Maybe<Pary_Variance_Fields>;
};


/** aggregate fields of "pary" */
export type Pary_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Pary_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "pary" */
export type Pary_Aggregate_Order_By = {
  avg?: InputMaybe<Pary_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Pary_Max_Order_By>;
  min?: InputMaybe<Pary_Min_Order_By>;
  stddev?: InputMaybe<Pary_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Pary_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Pary_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Pary_Sum_Order_By>;
  var_pop?: InputMaybe<Pary_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Pary_Var_Samp_Order_By>;
  variance?: InputMaybe<Pary_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "pary" */
export type Pary_Arr_Rel_Insert_Input = {
  data: Array<Pary_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Pary_On_Conflict>;
};

/** aggregate avg on columns */
export type Pary_Avg_Fields = {
  __typename?: 'pary_avg_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "pary" */
export type Pary_Avg_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "pary". All fields are combined with a logical 'AND'. */
export type Pary_Bool_Exp = {
  _and?: InputMaybe<Array<Pary_Bool_Exp>>;
  _not?: InputMaybe<Pary_Bool_Exp>;
  _or?: InputMaybe<Array<Pary_Bool_Exp>>;
  nabidka_items?: InputMaybe<Nabidka_Item_Bool_Exp>;
  p_archiv?: InputMaybe<Boolean_Comparison_Exp>;
  p_hodnoceni?: InputMaybe<Int_Comparison_Exp>;
  p_id?: InputMaybe<Bigint_Comparison_Exp>;
  p_id_partner?: InputMaybe<Bigint_Comparison_Exp>;
  p_id_partnerka?: InputMaybe<Bigint_Comparison_Exp>;
  p_lat_body?: InputMaybe<Int_Comparison_Exp>;
  p_lat_finale?: InputMaybe<Boolean_Comparison_Exp>;
  p_lat_trida?: InputMaybe<Pary_P_Lat_Trida_Comparison_Exp>;
  p_stt_body?: InputMaybe<Int_Comparison_Exp>;
  p_stt_finale?: InputMaybe<Boolean_Comparison_Exp>;
  p_stt_trida?: InputMaybe<Pary_P_Stt_Trida_Comparison_Exp>;
  p_timestamp_add?: InputMaybe<Timestamptz_Comparison_Exp>;
  p_timestamp_archive?: InputMaybe<Timestamptz_Comparison_Exp>;
  rozpis_items?: InputMaybe<Rozpis_Item_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "pary" */
export enum Pary_Constraint {
  /** unique or primary key constraint */
  Idx_24646Primary = 'idx_24646_primary'
}

/** input type for incrementing numeric columns in table "pary" */
export type Pary_Inc_Input = {
  p_hodnoceni?: InputMaybe<Scalars['Int']>;
  p_id?: InputMaybe<Scalars['bigint']>;
  p_id_partner?: InputMaybe<Scalars['bigint']>;
  p_id_partnerka?: InputMaybe<Scalars['bigint']>;
  p_lat_body?: InputMaybe<Scalars['Int']>;
  p_stt_body?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "pary" */
export type Pary_Insert_Input = {
  nabidka_items?: InputMaybe<Nabidka_Item_Arr_Rel_Insert_Input>;
  p_archiv?: InputMaybe<Scalars['Boolean']>;
  p_hodnoceni?: InputMaybe<Scalars['Int']>;
  p_id?: InputMaybe<Scalars['bigint']>;
  p_id_partner?: InputMaybe<Scalars['bigint']>;
  p_id_partnerka?: InputMaybe<Scalars['bigint']>;
  p_lat_body?: InputMaybe<Scalars['Int']>;
  p_lat_finale?: InputMaybe<Scalars['Boolean']>;
  p_lat_trida?: InputMaybe<Scalars['pary_p_lat_trida']>;
  p_stt_body?: InputMaybe<Scalars['Int']>;
  p_stt_finale?: InputMaybe<Scalars['Boolean']>;
  p_stt_trida?: InputMaybe<Scalars['pary_p_stt_trida']>;
  p_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
  p_timestamp_archive?: InputMaybe<Scalars['timestamptz']>;
  rozpis_items?: InputMaybe<Rozpis_Item_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Pary_Max_Fields = {
  __typename?: 'pary_max_fields';
  p_hodnoceni?: Maybe<Scalars['Int']>;
  p_id?: Maybe<Scalars['bigint']>;
  p_id_partner?: Maybe<Scalars['bigint']>;
  p_id_partnerka?: Maybe<Scalars['bigint']>;
  p_lat_body?: Maybe<Scalars['Int']>;
  p_stt_body?: Maybe<Scalars['Int']>;
  p_timestamp_add?: Maybe<Scalars['timestamptz']>;
  p_timestamp_archive?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "pary" */
export type Pary_Max_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
  p_timestamp_add?: InputMaybe<Order_By>;
  p_timestamp_archive?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Pary_Min_Fields = {
  __typename?: 'pary_min_fields';
  p_hodnoceni?: Maybe<Scalars['Int']>;
  p_id?: Maybe<Scalars['bigint']>;
  p_id_partner?: Maybe<Scalars['bigint']>;
  p_id_partnerka?: Maybe<Scalars['bigint']>;
  p_lat_body?: Maybe<Scalars['Int']>;
  p_stt_body?: Maybe<Scalars['Int']>;
  p_timestamp_add?: Maybe<Scalars['timestamptz']>;
  p_timestamp_archive?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "pary" */
export type Pary_Min_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
  p_timestamp_add?: InputMaybe<Order_By>;
  p_timestamp_archive?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "pary" */
export type Pary_Mutation_Response = {
  __typename?: 'pary_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Pary>;
};

/** columns and relationships of "pary_navrh" */
export type Pary_Navrh = {
  __typename?: 'pary_navrh';
  pn_id: Scalars['bigint'];
  pn_navrhl: Scalars['bigint'];
  pn_partner: Scalars['bigint'];
  pn_partnerka: Scalars['bigint'];
  /** An object relationship */
  user: Users;
  /** An object relationship */
  userByPnPartner: Users;
  /** An object relationship */
  userByPnPartnerka: Users;
};

/** aggregated selection of "pary_navrh" */
export type Pary_Navrh_Aggregate = {
  __typename?: 'pary_navrh_aggregate';
  aggregate?: Maybe<Pary_Navrh_Aggregate_Fields>;
  nodes: Array<Pary_Navrh>;
};

/** aggregate fields of "pary_navrh" */
export type Pary_Navrh_Aggregate_Fields = {
  __typename?: 'pary_navrh_aggregate_fields';
  avg?: Maybe<Pary_Navrh_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Pary_Navrh_Max_Fields>;
  min?: Maybe<Pary_Navrh_Min_Fields>;
  stddev?: Maybe<Pary_Navrh_Stddev_Fields>;
  stddev_pop?: Maybe<Pary_Navrh_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Pary_Navrh_Stddev_Samp_Fields>;
  sum?: Maybe<Pary_Navrh_Sum_Fields>;
  var_pop?: Maybe<Pary_Navrh_Var_Pop_Fields>;
  var_samp?: Maybe<Pary_Navrh_Var_Samp_Fields>;
  variance?: Maybe<Pary_Navrh_Variance_Fields>;
};


/** aggregate fields of "pary_navrh" */
export type Pary_Navrh_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "pary_navrh" */
export type Pary_Navrh_Aggregate_Order_By = {
  avg?: InputMaybe<Pary_Navrh_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Pary_Navrh_Max_Order_By>;
  min?: InputMaybe<Pary_Navrh_Min_Order_By>;
  stddev?: InputMaybe<Pary_Navrh_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Pary_Navrh_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Pary_Navrh_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Pary_Navrh_Sum_Order_By>;
  var_pop?: InputMaybe<Pary_Navrh_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Pary_Navrh_Var_Samp_Order_By>;
  variance?: InputMaybe<Pary_Navrh_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "pary_navrh" */
export type Pary_Navrh_Arr_Rel_Insert_Input = {
  data: Array<Pary_Navrh_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Pary_Navrh_On_Conflict>;
};

/** aggregate avg on columns */
export type Pary_Navrh_Avg_Fields = {
  __typename?: 'pary_navrh_avg_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "pary_navrh" */
export type Pary_Navrh_Avg_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "pary_navrh". All fields are combined with a logical 'AND'. */
export type Pary_Navrh_Bool_Exp = {
  _and?: InputMaybe<Array<Pary_Navrh_Bool_Exp>>;
  _not?: InputMaybe<Pary_Navrh_Bool_Exp>;
  _or?: InputMaybe<Array<Pary_Navrh_Bool_Exp>>;
  pn_id?: InputMaybe<Bigint_Comparison_Exp>;
  pn_navrhl?: InputMaybe<Bigint_Comparison_Exp>;
  pn_partner?: InputMaybe<Bigint_Comparison_Exp>;
  pn_partnerka?: InputMaybe<Bigint_Comparison_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
  userByPnPartner?: InputMaybe<Users_Bool_Exp>;
  userByPnPartnerka?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "pary_navrh" */
export enum Pary_Navrh_Constraint {
  /** unique or primary key constraint */
  Idx_24662Primary = 'idx_24662_primary'
}

/** input type for incrementing numeric columns in table "pary_navrh" */
export type Pary_Navrh_Inc_Input = {
  pn_id?: InputMaybe<Scalars['bigint']>;
  pn_navrhl?: InputMaybe<Scalars['bigint']>;
  pn_partner?: InputMaybe<Scalars['bigint']>;
  pn_partnerka?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "pary_navrh" */
export type Pary_Navrh_Insert_Input = {
  pn_id?: InputMaybe<Scalars['bigint']>;
  pn_navrhl?: InputMaybe<Scalars['bigint']>;
  pn_partner?: InputMaybe<Scalars['bigint']>;
  pn_partnerka?: InputMaybe<Scalars['bigint']>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
  userByPnPartner?: InputMaybe<Users_Obj_Rel_Insert_Input>;
  userByPnPartnerka?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Pary_Navrh_Max_Fields = {
  __typename?: 'pary_navrh_max_fields';
  pn_id?: Maybe<Scalars['bigint']>;
  pn_navrhl?: Maybe<Scalars['bigint']>;
  pn_partner?: Maybe<Scalars['bigint']>;
  pn_partnerka?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "pary_navrh" */
export type Pary_Navrh_Max_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Pary_Navrh_Min_Fields = {
  __typename?: 'pary_navrh_min_fields';
  pn_id?: Maybe<Scalars['bigint']>;
  pn_navrhl?: Maybe<Scalars['bigint']>;
  pn_partner?: Maybe<Scalars['bigint']>;
  pn_partnerka?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "pary_navrh" */
export type Pary_Navrh_Min_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "pary_navrh" */
export type Pary_Navrh_Mutation_Response = {
  __typename?: 'pary_navrh_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Pary_Navrh>;
};

/** on conflict condition type for table "pary_navrh" */
export type Pary_Navrh_On_Conflict = {
  constraint: Pary_Navrh_Constraint;
  update_columns?: Array<Pary_Navrh_Update_Column>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};

/** Ordering options when selecting data from "pary_navrh". */
export type Pary_Navrh_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
  user?: InputMaybe<Users_Order_By>;
  userByPnPartner?: InputMaybe<Users_Order_By>;
  userByPnPartnerka?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: pary_navrh */
export type Pary_Navrh_Pk_Columns_Input = {
  pn_id: Scalars['bigint'];
};

/** select columns of table "pary_navrh" */
export enum Pary_Navrh_Select_Column {
  /** column name */
  PnId = 'pn_id',
  /** column name */
  PnNavrhl = 'pn_navrhl',
  /** column name */
  PnPartner = 'pn_partner',
  /** column name */
  PnPartnerka = 'pn_partnerka'
}

/** input type for updating data in table "pary_navrh" */
export type Pary_Navrh_Set_Input = {
  pn_id?: InputMaybe<Scalars['bigint']>;
  pn_navrhl?: InputMaybe<Scalars['bigint']>;
  pn_partner?: InputMaybe<Scalars['bigint']>;
  pn_partnerka?: InputMaybe<Scalars['bigint']>;
};

/** aggregate stddev on columns */
export type Pary_Navrh_Stddev_Fields = {
  __typename?: 'pary_navrh_stddev_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "pary_navrh" */
export type Pary_Navrh_Stddev_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Pary_Navrh_Stddev_Pop_Fields = {
  __typename?: 'pary_navrh_stddev_pop_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "pary_navrh" */
export type Pary_Navrh_Stddev_Pop_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Pary_Navrh_Stddev_Samp_Fields = {
  __typename?: 'pary_navrh_stddev_samp_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "pary_navrh" */
export type Pary_Navrh_Stddev_Samp_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Pary_Navrh_Sum_Fields = {
  __typename?: 'pary_navrh_sum_fields';
  pn_id?: Maybe<Scalars['bigint']>;
  pn_navrhl?: Maybe<Scalars['bigint']>;
  pn_partner?: Maybe<Scalars['bigint']>;
  pn_partnerka?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "pary_navrh" */
export type Pary_Navrh_Sum_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** update columns of table "pary_navrh" */
export enum Pary_Navrh_Update_Column {
  /** column name */
  PnId = 'pn_id',
  /** column name */
  PnNavrhl = 'pn_navrhl',
  /** column name */
  PnPartner = 'pn_partner',
  /** column name */
  PnPartnerka = 'pn_partnerka'
}

/** aggregate var_pop on columns */
export type Pary_Navrh_Var_Pop_Fields = {
  __typename?: 'pary_navrh_var_pop_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "pary_navrh" */
export type Pary_Navrh_Var_Pop_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Pary_Navrh_Var_Samp_Fields = {
  __typename?: 'pary_navrh_var_samp_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "pary_navrh" */
export type Pary_Navrh_Var_Samp_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Pary_Navrh_Variance_Fields = {
  __typename?: 'pary_navrh_variance_fields';
  pn_id?: Maybe<Scalars['Float']>;
  pn_navrhl?: Maybe<Scalars['Float']>;
  pn_partner?: Maybe<Scalars['Float']>;
  pn_partnerka?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "pary_navrh" */
export type Pary_Navrh_Variance_Order_By = {
  pn_id?: InputMaybe<Order_By>;
  pn_navrhl?: InputMaybe<Order_By>;
  pn_partner?: InputMaybe<Order_By>;
  pn_partnerka?: InputMaybe<Order_By>;
};

/** input type for inserting object relation for remote table "pary" */
export type Pary_Obj_Rel_Insert_Input = {
  data: Pary_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Pary_On_Conflict>;
};

/** on conflict condition type for table "pary" */
export type Pary_On_Conflict = {
  constraint: Pary_Constraint;
  update_columns?: Array<Pary_Update_Column>;
  where?: InputMaybe<Pary_Bool_Exp>;
};

/** Ordering options when selecting data from "pary". */
export type Pary_Order_By = {
  nabidka_items_aggregate?: InputMaybe<Nabidka_Item_Aggregate_Order_By>;
  p_archiv?: InputMaybe<Order_By>;
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_lat_finale?: InputMaybe<Order_By>;
  p_lat_trida?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
  p_stt_finale?: InputMaybe<Order_By>;
  p_stt_trida?: InputMaybe<Order_By>;
  p_timestamp_add?: InputMaybe<Order_By>;
  p_timestamp_archive?: InputMaybe<Order_By>;
  rozpis_items_aggregate?: InputMaybe<Rozpis_Item_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** Boolean expression to compare columns of type "pary_p_lat_trida". All fields are combined with logical 'AND'. */
export type Pary_P_Lat_Trida_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _gt?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _gte?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _in?: InputMaybe<Array<Scalars['pary_p_lat_trida']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _lte?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _neq?: InputMaybe<Scalars['pary_p_lat_trida']>;
  _nin?: InputMaybe<Array<Scalars['pary_p_lat_trida']>>;
};

/** Boolean expression to compare columns of type "pary_p_stt_trida". All fields are combined with logical 'AND'. */
export type Pary_P_Stt_Trida_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _gt?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _gte?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _in?: InputMaybe<Array<Scalars['pary_p_stt_trida']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _lte?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _neq?: InputMaybe<Scalars['pary_p_stt_trida']>;
  _nin?: InputMaybe<Array<Scalars['pary_p_stt_trida']>>;
};

/** primary key columns input for table: pary */
export type Pary_Pk_Columns_Input = {
  p_id: Scalars['bigint'];
};

/** select columns of table "pary" */
export enum Pary_Select_Column {
  /** column name */
  PArchiv = 'p_archiv',
  /** column name */
  PHodnoceni = 'p_hodnoceni',
  /** column name */
  PId = 'p_id',
  /** column name */
  PIdPartner = 'p_id_partner',
  /** column name */
  PIdPartnerka = 'p_id_partnerka',
  /** column name */
  PLatBody = 'p_lat_body',
  /** column name */
  PLatFinale = 'p_lat_finale',
  /** column name */
  PLatTrida = 'p_lat_trida',
  /** column name */
  PSttBody = 'p_stt_body',
  /** column name */
  PSttFinale = 'p_stt_finale',
  /** column name */
  PSttTrida = 'p_stt_trida',
  /** column name */
  PTimestampAdd = 'p_timestamp_add',
  /** column name */
  PTimestampArchive = 'p_timestamp_archive'
}

/** input type for updating data in table "pary" */
export type Pary_Set_Input = {
  p_archiv?: InputMaybe<Scalars['Boolean']>;
  p_hodnoceni?: InputMaybe<Scalars['Int']>;
  p_id?: InputMaybe<Scalars['bigint']>;
  p_id_partner?: InputMaybe<Scalars['bigint']>;
  p_id_partnerka?: InputMaybe<Scalars['bigint']>;
  p_lat_body?: InputMaybe<Scalars['Int']>;
  p_lat_finale?: InputMaybe<Scalars['Boolean']>;
  p_lat_trida?: InputMaybe<Scalars['pary_p_lat_trida']>;
  p_stt_body?: InputMaybe<Scalars['Int']>;
  p_stt_finale?: InputMaybe<Scalars['Boolean']>;
  p_stt_trida?: InputMaybe<Scalars['pary_p_stt_trida']>;
  p_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
  p_timestamp_archive?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Pary_Stddev_Fields = {
  __typename?: 'pary_stddev_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "pary" */
export type Pary_Stddev_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Pary_Stddev_Pop_Fields = {
  __typename?: 'pary_stddev_pop_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "pary" */
export type Pary_Stddev_Pop_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Pary_Stddev_Samp_Fields = {
  __typename?: 'pary_stddev_samp_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "pary" */
export type Pary_Stddev_Samp_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Pary_Sum_Fields = {
  __typename?: 'pary_sum_fields';
  p_hodnoceni?: Maybe<Scalars['Int']>;
  p_id?: Maybe<Scalars['bigint']>;
  p_id_partner?: Maybe<Scalars['bigint']>;
  p_id_partnerka?: Maybe<Scalars['bigint']>;
  p_lat_body?: Maybe<Scalars['Int']>;
  p_stt_body?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "pary" */
export type Pary_Sum_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** update columns of table "pary" */
export enum Pary_Update_Column {
  /** column name */
  PArchiv = 'p_archiv',
  /** column name */
  PHodnoceni = 'p_hodnoceni',
  /** column name */
  PId = 'p_id',
  /** column name */
  PIdPartner = 'p_id_partner',
  /** column name */
  PIdPartnerka = 'p_id_partnerka',
  /** column name */
  PLatBody = 'p_lat_body',
  /** column name */
  PLatFinale = 'p_lat_finale',
  /** column name */
  PLatTrida = 'p_lat_trida',
  /** column name */
  PSttBody = 'p_stt_body',
  /** column name */
  PSttFinale = 'p_stt_finale',
  /** column name */
  PSttTrida = 'p_stt_trida',
  /** column name */
  PTimestampAdd = 'p_timestamp_add',
  /** column name */
  PTimestampArchive = 'p_timestamp_archive'
}

/** aggregate var_pop on columns */
export type Pary_Var_Pop_Fields = {
  __typename?: 'pary_var_pop_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "pary" */
export type Pary_Var_Pop_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Pary_Var_Samp_Fields = {
  __typename?: 'pary_var_samp_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "pary" */
export type Pary_Var_Samp_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Pary_Variance_Fields = {
  __typename?: 'pary_variance_fields';
  p_hodnoceni?: Maybe<Scalars['Float']>;
  p_id?: Maybe<Scalars['Float']>;
  p_id_partner?: Maybe<Scalars['Float']>;
  p_id_partnerka?: Maybe<Scalars['Float']>;
  p_lat_body?: Maybe<Scalars['Float']>;
  p_stt_body?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "pary" */
export type Pary_Variance_Order_By = {
  p_hodnoceni?: InputMaybe<Order_By>;
  p_id?: InputMaybe<Order_By>;
  p_id_partner?: InputMaybe<Order_By>;
  p_id_partnerka?: InputMaybe<Order_By>;
  p_lat_body?: InputMaybe<Order_By>;
  p_stt_body?: InputMaybe<Order_By>;
};

/** columns and relationships of "permissions" */
export type Permissions = {
  __typename?: 'permissions';
  pe_akce: Scalars['Int'];
  pe_aktuality: Scalars['Int'];
  pe_ankety: Scalars['Int'];
  pe_description: Scalars['String'];
  pe_dokumenty: Scalars['Int'];
  pe_galerie: Scalars['Int'];
  pe_id: Scalars['bigint'];
  pe_inzerce: Scalars['Int'];
  pe_konzole: Scalars['Int'];
  pe_main: Scalars['Int'];
  pe_nabidka: Scalars['Int'];
  pe_name: Scalars['String'];
  pe_nastenka: Scalars['Int'];
  pe_novinky: Scalars['Int'];
  pe_pary: Scalars['Int'];
  pe_permissions: Scalars['Int'];
  pe_platby: Scalars['Int'];
  pe_rozpis: Scalars['Int'];
  pe_skupiny: Scalars['Int'];
  pe_users: Scalars['Int'];
  /** An array relationship */
  users: Array<Users>;
  /** An aggregate relationship */
  users_aggregate: Users_Aggregate;
};


/** columns and relationships of "permissions" */
export type PermissionsUsersArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


/** columns and relationships of "permissions" */
export type PermissionsUsers_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};

/** aggregated selection of "permissions" */
export type Permissions_Aggregate = {
  __typename?: 'permissions_aggregate';
  aggregate?: Maybe<Permissions_Aggregate_Fields>;
  nodes: Array<Permissions>;
};

/** aggregate fields of "permissions" */
export type Permissions_Aggregate_Fields = {
  __typename?: 'permissions_aggregate_fields';
  avg?: Maybe<Permissions_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Permissions_Max_Fields>;
  min?: Maybe<Permissions_Min_Fields>;
  stddev?: Maybe<Permissions_Stddev_Fields>;
  stddev_pop?: Maybe<Permissions_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Permissions_Stddev_Samp_Fields>;
  sum?: Maybe<Permissions_Sum_Fields>;
  var_pop?: Maybe<Permissions_Var_Pop_Fields>;
  var_samp?: Maybe<Permissions_Var_Samp_Fields>;
  variance?: Maybe<Permissions_Variance_Fields>;
};


/** aggregate fields of "permissions" */
export type Permissions_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Permissions_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Permissions_Avg_Fields = {
  __typename?: 'permissions_avg_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "permissions". All fields are combined with a logical 'AND'. */
export type Permissions_Bool_Exp = {
  _and?: InputMaybe<Array<Permissions_Bool_Exp>>;
  _not?: InputMaybe<Permissions_Bool_Exp>;
  _or?: InputMaybe<Array<Permissions_Bool_Exp>>;
  pe_akce?: InputMaybe<Int_Comparison_Exp>;
  pe_aktuality?: InputMaybe<Int_Comparison_Exp>;
  pe_ankety?: InputMaybe<Int_Comparison_Exp>;
  pe_description?: InputMaybe<String_Comparison_Exp>;
  pe_dokumenty?: InputMaybe<Int_Comparison_Exp>;
  pe_galerie?: InputMaybe<Int_Comparison_Exp>;
  pe_id?: InputMaybe<Bigint_Comparison_Exp>;
  pe_inzerce?: InputMaybe<Int_Comparison_Exp>;
  pe_konzole?: InputMaybe<Int_Comparison_Exp>;
  pe_main?: InputMaybe<Int_Comparison_Exp>;
  pe_nabidka?: InputMaybe<Int_Comparison_Exp>;
  pe_name?: InputMaybe<String_Comparison_Exp>;
  pe_nastenka?: InputMaybe<Int_Comparison_Exp>;
  pe_novinky?: InputMaybe<Int_Comparison_Exp>;
  pe_pary?: InputMaybe<Int_Comparison_Exp>;
  pe_permissions?: InputMaybe<Int_Comparison_Exp>;
  pe_platby?: InputMaybe<Int_Comparison_Exp>;
  pe_rozpis?: InputMaybe<Int_Comparison_Exp>;
  pe_skupiny?: InputMaybe<Int_Comparison_Exp>;
  pe_users?: InputMaybe<Int_Comparison_Exp>;
  users?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "permissions" */
export enum Permissions_Constraint {
  /** unique or primary key constraint */
  Idx_24668Primary = 'idx_24668_primary'
}

/** input type for incrementing numeric columns in table "permissions" */
export type Permissions_Inc_Input = {
  pe_akce?: InputMaybe<Scalars['Int']>;
  pe_aktuality?: InputMaybe<Scalars['Int']>;
  pe_ankety?: InputMaybe<Scalars['Int']>;
  pe_dokumenty?: InputMaybe<Scalars['Int']>;
  pe_galerie?: InputMaybe<Scalars['Int']>;
  pe_id?: InputMaybe<Scalars['bigint']>;
  pe_inzerce?: InputMaybe<Scalars['Int']>;
  pe_konzole?: InputMaybe<Scalars['Int']>;
  pe_main?: InputMaybe<Scalars['Int']>;
  pe_nabidka?: InputMaybe<Scalars['Int']>;
  pe_nastenka?: InputMaybe<Scalars['Int']>;
  pe_novinky?: InputMaybe<Scalars['Int']>;
  pe_pary?: InputMaybe<Scalars['Int']>;
  pe_permissions?: InputMaybe<Scalars['Int']>;
  pe_platby?: InputMaybe<Scalars['Int']>;
  pe_rozpis?: InputMaybe<Scalars['Int']>;
  pe_skupiny?: InputMaybe<Scalars['Int']>;
  pe_users?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "permissions" */
export type Permissions_Insert_Input = {
  pe_akce?: InputMaybe<Scalars['Int']>;
  pe_aktuality?: InputMaybe<Scalars['Int']>;
  pe_ankety?: InputMaybe<Scalars['Int']>;
  pe_description?: InputMaybe<Scalars['String']>;
  pe_dokumenty?: InputMaybe<Scalars['Int']>;
  pe_galerie?: InputMaybe<Scalars['Int']>;
  pe_id?: InputMaybe<Scalars['bigint']>;
  pe_inzerce?: InputMaybe<Scalars['Int']>;
  pe_konzole?: InputMaybe<Scalars['Int']>;
  pe_main?: InputMaybe<Scalars['Int']>;
  pe_nabidka?: InputMaybe<Scalars['Int']>;
  pe_name?: InputMaybe<Scalars['String']>;
  pe_nastenka?: InputMaybe<Scalars['Int']>;
  pe_novinky?: InputMaybe<Scalars['Int']>;
  pe_pary?: InputMaybe<Scalars['Int']>;
  pe_permissions?: InputMaybe<Scalars['Int']>;
  pe_platby?: InputMaybe<Scalars['Int']>;
  pe_rozpis?: InputMaybe<Scalars['Int']>;
  pe_skupiny?: InputMaybe<Scalars['Int']>;
  pe_users?: InputMaybe<Scalars['Int']>;
  users?: InputMaybe<Users_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Permissions_Max_Fields = {
  __typename?: 'permissions_max_fields';
  pe_akce?: Maybe<Scalars['Int']>;
  pe_aktuality?: Maybe<Scalars['Int']>;
  pe_ankety?: Maybe<Scalars['Int']>;
  pe_description?: Maybe<Scalars['String']>;
  pe_dokumenty?: Maybe<Scalars['Int']>;
  pe_galerie?: Maybe<Scalars['Int']>;
  pe_id?: Maybe<Scalars['bigint']>;
  pe_inzerce?: Maybe<Scalars['Int']>;
  pe_konzole?: Maybe<Scalars['Int']>;
  pe_main?: Maybe<Scalars['Int']>;
  pe_nabidka?: Maybe<Scalars['Int']>;
  pe_name?: Maybe<Scalars['String']>;
  pe_nastenka?: Maybe<Scalars['Int']>;
  pe_novinky?: Maybe<Scalars['Int']>;
  pe_pary?: Maybe<Scalars['Int']>;
  pe_permissions?: Maybe<Scalars['Int']>;
  pe_platby?: Maybe<Scalars['Int']>;
  pe_rozpis?: Maybe<Scalars['Int']>;
  pe_skupiny?: Maybe<Scalars['Int']>;
  pe_users?: Maybe<Scalars['Int']>;
};

/** aggregate min on columns */
export type Permissions_Min_Fields = {
  __typename?: 'permissions_min_fields';
  pe_akce?: Maybe<Scalars['Int']>;
  pe_aktuality?: Maybe<Scalars['Int']>;
  pe_ankety?: Maybe<Scalars['Int']>;
  pe_description?: Maybe<Scalars['String']>;
  pe_dokumenty?: Maybe<Scalars['Int']>;
  pe_galerie?: Maybe<Scalars['Int']>;
  pe_id?: Maybe<Scalars['bigint']>;
  pe_inzerce?: Maybe<Scalars['Int']>;
  pe_konzole?: Maybe<Scalars['Int']>;
  pe_main?: Maybe<Scalars['Int']>;
  pe_nabidka?: Maybe<Scalars['Int']>;
  pe_name?: Maybe<Scalars['String']>;
  pe_nastenka?: Maybe<Scalars['Int']>;
  pe_novinky?: Maybe<Scalars['Int']>;
  pe_pary?: Maybe<Scalars['Int']>;
  pe_permissions?: Maybe<Scalars['Int']>;
  pe_platby?: Maybe<Scalars['Int']>;
  pe_rozpis?: Maybe<Scalars['Int']>;
  pe_skupiny?: Maybe<Scalars['Int']>;
  pe_users?: Maybe<Scalars['Int']>;
};

/** response of any mutation on the table "permissions" */
export type Permissions_Mutation_Response = {
  __typename?: 'permissions_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Permissions>;
};

/** input type for inserting object relation for remote table "permissions" */
export type Permissions_Obj_Rel_Insert_Input = {
  data: Permissions_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Permissions_On_Conflict>;
};

/** on conflict condition type for table "permissions" */
export type Permissions_On_Conflict = {
  constraint: Permissions_Constraint;
  update_columns?: Array<Permissions_Update_Column>;
  where?: InputMaybe<Permissions_Bool_Exp>;
};

/** Ordering options when selecting data from "permissions". */
export type Permissions_Order_By = {
  pe_akce?: InputMaybe<Order_By>;
  pe_aktuality?: InputMaybe<Order_By>;
  pe_ankety?: InputMaybe<Order_By>;
  pe_description?: InputMaybe<Order_By>;
  pe_dokumenty?: InputMaybe<Order_By>;
  pe_galerie?: InputMaybe<Order_By>;
  pe_id?: InputMaybe<Order_By>;
  pe_inzerce?: InputMaybe<Order_By>;
  pe_konzole?: InputMaybe<Order_By>;
  pe_main?: InputMaybe<Order_By>;
  pe_nabidka?: InputMaybe<Order_By>;
  pe_name?: InputMaybe<Order_By>;
  pe_nastenka?: InputMaybe<Order_By>;
  pe_novinky?: InputMaybe<Order_By>;
  pe_pary?: InputMaybe<Order_By>;
  pe_permissions?: InputMaybe<Order_By>;
  pe_platby?: InputMaybe<Order_By>;
  pe_rozpis?: InputMaybe<Order_By>;
  pe_skupiny?: InputMaybe<Order_By>;
  pe_users?: InputMaybe<Order_By>;
  users_aggregate?: InputMaybe<Users_Aggregate_Order_By>;
};

/** primary key columns input for table: permissions */
export type Permissions_Pk_Columns_Input = {
  pe_id: Scalars['bigint'];
};

/** select columns of table "permissions" */
export enum Permissions_Select_Column {
  /** column name */
  PeAkce = 'pe_akce',
  /** column name */
  PeAktuality = 'pe_aktuality',
  /** column name */
  PeAnkety = 'pe_ankety',
  /** column name */
  PeDescription = 'pe_description',
  /** column name */
  PeDokumenty = 'pe_dokumenty',
  /** column name */
  PeGalerie = 'pe_galerie',
  /** column name */
  PeId = 'pe_id',
  /** column name */
  PeInzerce = 'pe_inzerce',
  /** column name */
  PeKonzole = 'pe_konzole',
  /** column name */
  PeMain = 'pe_main',
  /** column name */
  PeNabidka = 'pe_nabidka',
  /** column name */
  PeName = 'pe_name',
  /** column name */
  PeNastenka = 'pe_nastenka',
  /** column name */
  PeNovinky = 'pe_novinky',
  /** column name */
  PePary = 'pe_pary',
  /** column name */
  PePermissions = 'pe_permissions',
  /** column name */
  PePlatby = 'pe_platby',
  /** column name */
  PeRozpis = 'pe_rozpis',
  /** column name */
  PeSkupiny = 'pe_skupiny',
  /** column name */
  PeUsers = 'pe_users'
}

/** input type for updating data in table "permissions" */
export type Permissions_Set_Input = {
  pe_akce?: InputMaybe<Scalars['Int']>;
  pe_aktuality?: InputMaybe<Scalars['Int']>;
  pe_ankety?: InputMaybe<Scalars['Int']>;
  pe_description?: InputMaybe<Scalars['String']>;
  pe_dokumenty?: InputMaybe<Scalars['Int']>;
  pe_galerie?: InputMaybe<Scalars['Int']>;
  pe_id?: InputMaybe<Scalars['bigint']>;
  pe_inzerce?: InputMaybe<Scalars['Int']>;
  pe_konzole?: InputMaybe<Scalars['Int']>;
  pe_main?: InputMaybe<Scalars['Int']>;
  pe_nabidka?: InputMaybe<Scalars['Int']>;
  pe_name?: InputMaybe<Scalars['String']>;
  pe_nastenka?: InputMaybe<Scalars['Int']>;
  pe_novinky?: InputMaybe<Scalars['Int']>;
  pe_pary?: InputMaybe<Scalars['Int']>;
  pe_permissions?: InputMaybe<Scalars['Int']>;
  pe_platby?: InputMaybe<Scalars['Int']>;
  pe_rozpis?: InputMaybe<Scalars['Int']>;
  pe_skupiny?: InputMaybe<Scalars['Int']>;
  pe_users?: InputMaybe<Scalars['Int']>;
};

/** aggregate stddev on columns */
export type Permissions_Stddev_Fields = {
  __typename?: 'permissions_stddev_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Permissions_Stddev_Pop_Fields = {
  __typename?: 'permissions_stddev_pop_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Permissions_Stddev_Samp_Fields = {
  __typename?: 'permissions_stddev_samp_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Permissions_Sum_Fields = {
  __typename?: 'permissions_sum_fields';
  pe_akce?: Maybe<Scalars['Int']>;
  pe_aktuality?: Maybe<Scalars['Int']>;
  pe_ankety?: Maybe<Scalars['Int']>;
  pe_dokumenty?: Maybe<Scalars['Int']>;
  pe_galerie?: Maybe<Scalars['Int']>;
  pe_id?: Maybe<Scalars['bigint']>;
  pe_inzerce?: Maybe<Scalars['Int']>;
  pe_konzole?: Maybe<Scalars['Int']>;
  pe_main?: Maybe<Scalars['Int']>;
  pe_nabidka?: Maybe<Scalars['Int']>;
  pe_nastenka?: Maybe<Scalars['Int']>;
  pe_novinky?: Maybe<Scalars['Int']>;
  pe_pary?: Maybe<Scalars['Int']>;
  pe_permissions?: Maybe<Scalars['Int']>;
  pe_platby?: Maybe<Scalars['Int']>;
  pe_rozpis?: Maybe<Scalars['Int']>;
  pe_skupiny?: Maybe<Scalars['Int']>;
  pe_users?: Maybe<Scalars['Int']>;
};

/** update columns of table "permissions" */
export enum Permissions_Update_Column {
  /** column name */
  PeAkce = 'pe_akce',
  /** column name */
  PeAktuality = 'pe_aktuality',
  /** column name */
  PeAnkety = 'pe_ankety',
  /** column name */
  PeDescription = 'pe_description',
  /** column name */
  PeDokumenty = 'pe_dokumenty',
  /** column name */
  PeGalerie = 'pe_galerie',
  /** column name */
  PeId = 'pe_id',
  /** column name */
  PeInzerce = 'pe_inzerce',
  /** column name */
  PeKonzole = 'pe_konzole',
  /** column name */
  PeMain = 'pe_main',
  /** column name */
  PeNabidka = 'pe_nabidka',
  /** column name */
  PeName = 'pe_name',
  /** column name */
  PeNastenka = 'pe_nastenka',
  /** column name */
  PeNovinky = 'pe_novinky',
  /** column name */
  PePary = 'pe_pary',
  /** column name */
  PePermissions = 'pe_permissions',
  /** column name */
  PePlatby = 'pe_platby',
  /** column name */
  PeRozpis = 'pe_rozpis',
  /** column name */
  PeSkupiny = 'pe_skupiny',
  /** column name */
  PeUsers = 'pe_users'
}

/** aggregate var_pop on columns */
export type Permissions_Var_Pop_Fields = {
  __typename?: 'permissions_var_pop_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Permissions_Var_Samp_Fields = {
  __typename?: 'permissions_var_samp_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Permissions_Variance_Fields = {
  __typename?: 'permissions_variance_fields';
  pe_akce?: Maybe<Scalars['Float']>;
  pe_aktuality?: Maybe<Scalars['Float']>;
  pe_ankety?: Maybe<Scalars['Float']>;
  pe_dokumenty?: Maybe<Scalars['Float']>;
  pe_galerie?: Maybe<Scalars['Float']>;
  pe_id?: Maybe<Scalars['Float']>;
  pe_inzerce?: Maybe<Scalars['Float']>;
  pe_konzole?: Maybe<Scalars['Float']>;
  pe_main?: Maybe<Scalars['Float']>;
  pe_nabidka?: Maybe<Scalars['Float']>;
  pe_nastenka?: Maybe<Scalars['Float']>;
  pe_novinky?: Maybe<Scalars['Float']>;
  pe_pary?: Maybe<Scalars['Float']>;
  pe_permissions?: Maybe<Scalars['Float']>;
  pe_platby?: Maybe<Scalars['Float']>;
  pe_rozpis?: Maybe<Scalars['Float']>;
  pe_skupiny?: Maybe<Scalars['Float']>;
  pe_users?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "platby_category" */
export type Platby_Category = {
  __typename?: 'platby_category';
  pc_amount: Scalars['numeric'];
  pc_archive: Scalars['Boolean'];
  pc_date_due: Scalars['date'];
  pc_id: Scalars['bigint'];
  pc_name: Scalars['String'];
  pc_symbol: Scalars['bigint'];
  pc_use_base: Scalars['Boolean'];
  pc_use_prefix: Scalars['Boolean'];
  pc_valid_from: Scalars['date'];
  pc_valid_to: Scalars['date'];
  pc_visible: Scalars['Boolean'];
  /** An array relationship */
  platby_category_groups: Array<Platby_Category_Group>;
  /** An aggregate relationship */
  platby_category_groups_aggregate: Platby_Category_Group_Aggregate;
  /** An array relationship */
  platby_items: Array<Platby_Item>;
  /** An aggregate relationship */
  platby_items_aggregate: Platby_Item_Aggregate;
};


/** columns and relationships of "platby_category" */
export type Platby_CategoryPlatby_Category_GroupsArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


/** columns and relationships of "platby_category" */
export type Platby_CategoryPlatby_Category_Groups_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


/** columns and relationships of "platby_category" */
export type Platby_CategoryPlatby_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


/** columns and relationships of "platby_category" */
export type Platby_CategoryPlatby_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};

/** aggregated selection of "platby_category" */
export type Platby_Category_Aggregate = {
  __typename?: 'platby_category_aggregate';
  aggregate?: Maybe<Platby_Category_Aggregate_Fields>;
  nodes: Array<Platby_Category>;
};

/** aggregate fields of "platby_category" */
export type Platby_Category_Aggregate_Fields = {
  __typename?: 'platby_category_aggregate_fields';
  avg?: Maybe<Platby_Category_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Category_Max_Fields>;
  min?: Maybe<Platby_Category_Min_Fields>;
  stddev?: Maybe<Platby_Category_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Category_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Category_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Category_Sum_Fields>;
  var_pop?: Maybe<Platby_Category_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Category_Var_Samp_Fields>;
  variance?: Maybe<Platby_Category_Variance_Fields>;
};


/** aggregate fields of "platby_category" */
export type Platby_Category_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Category_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Platby_Category_Avg_Fields = {
  __typename?: 'platby_category_avg_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "platby_category". All fields are combined with a logical 'AND'. */
export type Platby_Category_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Category_Bool_Exp>>;
  _not?: InputMaybe<Platby_Category_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Category_Bool_Exp>>;
  pc_amount?: InputMaybe<Numeric_Comparison_Exp>;
  pc_archive?: InputMaybe<Boolean_Comparison_Exp>;
  pc_date_due?: InputMaybe<Date_Comparison_Exp>;
  pc_id?: InputMaybe<Bigint_Comparison_Exp>;
  pc_name?: InputMaybe<String_Comparison_Exp>;
  pc_symbol?: InputMaybe<Bigint_Comparison_Exp>;
  pc_use_base?: InputMaybe<Boolean_Comparison_Exp>;
  pc_use_prefix?: InputMaybe<Boolean_Comparison_Exp>;
  pc_valid_from?: InputMaybe<Date_Comparison_Exp>;
  pc_valid_to?: InputMaybe<Date_Comparison_Exp>;
  pc_visible?: InputMaybe<Boolean_Comparison_Exp>;
  platby_category_groups?: InputMaybe<Platby_Category_Group_Bool_Exp>;
  platby_items?: InputMaybe<Platby_Item_Bool_Exp>;
};

/** unique or primary key constraints on table "platby_category" */
export enum Platby_Category_Constraint {
  /** unique or primary key constraint */
  Idx_24677PcSymbol = 'idx_24677_pc_symbol',
  /** unique or primary key constraint */
  Idx_24677Primary = 'idx_24677_primary'
}

/** columns and relationships of "platby_category_group" */
export type Platby_Category_Group = {
  __typename?: 'platby_category_group';
  pcg_id: Scalars['bigint'];
  pcg_id_category: Scalars['bigint'];
  pcg_id_group: Scalars['bigint'];
  /** An object relationship */
  platby_category: Platby_Category;
  /** An object relationship */
  platby_group: Platby_Group;
};

/** aggregated selection of "platby_category_group" */
export type Platby_Category_Group_Aggregate = {
  __typename?: 'platby_category_group_aggregate';
  aggregate?: Maybe<Platby_Category_Group_Aggregate_Fields>;
  nodes: Array<Platby_Category_Group>;
};

/** aggregate fields of "platby_category_group" */
export type Platby_Category_Group_Aggregate_Fields = {
  __typename?: 'platby_category_group_aggregate_fields';
  avg?: Maybe<Platby_Category_Group_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Category_Group_Max_Fields>;
  min?: Maybe<Platby_Category_Group_Min_Fields>;
  stddev?: Maybe<Platby_Category_Group_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Category_Group_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Category_Group_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Category_Group_Sum_Fields>;
  var_pop?: Maybe<Platby_Category_Group_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Category_Group_Var_Samp_Fields>;
  variance?: Maybe<Platby_Category_Group_Variance_Fields>;
};


/** aggregate fields of "platby_category_group" */
export type Platby_Category_Group_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "platby_category_group" */
export type Platby_Category_Group_Aggregate_Order_By = {
  avg?: InputMaybe<Platby_Category_Group_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Platby_Category_Group_Max_Order_By>;
  min?: InputMaybe<Platby_Category_Group_Min_Order_By>;
  stddev?: InputMaybe<Platby_Category_Group_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Platby_Category_Group_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Platby_Category_Group_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Platby_Category_Group_Sum_Order_By>;
  var_pop?: InputMaybe<Platby_Category_Group_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Platby_Category_Group_Var_Samp_Order_By>;
  variance?: InputMaybe<Platby_Category_Group_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "platby_category_group" */
export type Platby_Category_Group_Arr_Rel_Insert_Input = {
  data: Array<Platby_Category_Group_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Category_Group_On_Conflict>;
};

/** aggregate avg on columns */
export type Platby_Category_Group_Avg_Fields = {
  __typename?: 'platby_category_group_avg_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "platby_category_group" */
export type Platby_Category_Group_Avg_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "platby_category_group". All fields are combined with a logical 'AND'. */
export type Platby_Category_Group_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Category_Group_Bool_Exp>>;
  _not?: InputMaybe<Platby_Category_Group_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Category_Group_Bool_Exp>>;
  pcg_id?: InputMaybe<Bigint_Comparison_Exp>;
  pcg_id_category?: InputMaybe<Bigint_Comparison_Exp>;
  pcg_id_group?: InputMaybe<Bigint_Comparison_Exp>;
  platby_category?: InputMaybe<Platby_Category_Bool_Exp>;
  platby_group?: InputMaybe<Platby_Group_Bool_Exp>;
};

/** unique or primary key constraints on table "platby_category_group" */
export enum Platby_Category_Group_Constraint {
  /** unique or primary key constraint */
  Idx_24690PcgIdGroup = 'idx_24690_pcg_id_group',
  /** unique or primary key constraint */
  Idx_24690Primary = 'idx_24690_primary'
}

/** input type for incrementing numeric columns in table "platby_category_group" */
export type Platby_Category_Group_Inc_Input = {
  pcg_id?: InputMaybe<Scalars['bigint']>;
  pcg_id_category?: InputMaybe<Scalars['bigint']>;
  pcg_id_group?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "platby_category_group" */
export type Platby_Category_Group_Insert_Input = {
  pcg_id?: InputMaybe<Scalars['bigint']>;
  pcg_id_category?: InputMaybe<Scalars['bigint']>;
  pcg_id_group?: InputMaybe<Scalars['bigint']>;
  platby_category?: InputMaybe<Platby_Category_Obj_Rel_Insert_Input>;
  platby_group?: InputMaybe<Platby_Group_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Platby_Category_Group_Max_Fields = {
  __typename?: 'platby_category_group_max_fields';
  pcg_id?: Maybe<Scalars['bigint']>;
  pcg_id_category?: Maybe<Scalars['bigint']>;
  pcg_id_group?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "platby_category_group" */
export type Platby_Category_Group_Max_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Platby_Category_Group_Min_Fields = {
  __typename?: 'platby_category_group_min_fields';
  pcg_id?: Maybe<Scalars['bigint']>;
  pcg_id_category?: Maybe<Scalars['bigint']>;
  pcg_id_group?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "platby_category_group" */
export type Platby_Category_Group_Min_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "platby_category_group" */
export type Platby_Category_Group_Mutation_Response = {
  __typename?: 'platby_category_group_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Category_Group>;
};

/** on conflict condition type for table "platby_category_group" */
export type Platby_Category_Group_On_Conflict = {
  constraint: Platby_Category_Group_Constraint;
  update_columns?: Array<Platby_Category_Group_Update_Column>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_category_group". */
export type Platby_Category_Group_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
  platby_category?: InputMaybe<Platby_Category_Order_By>;
  platby_group?: InputMaybe<Platby_Group_Order_By>;
};

/** primary key columns input for table: platby_category_group */
export type Platby_Category_Group_Pk_Columns_Input = {
  pcg_id: Scalars['bigint'];
};

/** select columns of table "platby_category_group" */
export enum Platby_Category_Group_Select_Column {
  /** column name */
  PcgId = 'pcg_id',
  /** column name */
  PcgIdCategory = 'pcg_id_category',
  /** column name */
  PcgIdGroup = 'pcg_id_group'
}

/** input type for updating data in table "platby_category_group" */
export type Platby_Category_Group_Set_Input = {
  pcg_id?: InputMaybe<Scalars['bigint']>;
  pcg_id_category?: InputMaybe<Scalars['bigint']>;
  pcg_id_group?: InputMaybe<Scalars['bigint']>;
};

/** aggregate stddev on columns */
export type Platby_Category_Group_Stddev_Fields = {
  __typename?: 'platby_category_group_stddev_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "platby_category_group" */
export type Platby_Category_Group_Stddev_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Platby_Category_Group_Stddev_Pop_Fields = {
  __typename?: 'platby_category_group_stddev_pop_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "platby_category_group" */
export type Platby_Category_Group_Stddev_Pop_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Platby_Category_Group_Stddev_Samp_Fields = {
  __typename?: 'platby_category_group_stddev_samp_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "platby_category_group" */
export type Platby_Category_Group_Stddev_Samp_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Platby_Category_Group_Sum_Fields = {
  __typename?: 'platby_category_group_sum_fields';
  pcg_id?: Maybe<Scalars['bigint']>;
  pcg_id_category?: Maybe<Scalars['bigint']>;
  pcg_id_group?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "platby_category_group" */
export type Platby_Category_Group_Sum_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** update columns of table "platby_category_group" */
export enum Platby_Category_Group_Update_Column {
  /** column name */
  PcgId = 'pcg_id',
  /** column name */
  PcgIdCategory = 'pcg_id_category',
  /** column name */
  PcgIdGroup = 'pcg_id_group'
}

/** aggregate var_pop on columns */
export type Platby_Category_Group_Var_Pop_Fields = {
  __typename?: 'platby_category_group_var_pop_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "platby_category_group" */
export type Platby_Category_Group_Var_Pop_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Platby_Category_Group_Var_Samp_Fields = {
  __typename?: 'platby_category_group_var_samp_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "platby_category_group" */
export type Platby_Category_Group_Var_Samp_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Platby_Category_Group_Variance_Fields = {
  __typename?: 'platby_category_group_variance_fields';
  pcg_id?: Maybe<Scalars['Float']>;
  pcg_id_category?: Maybe<Scalars['Float']>;
  pcg_id_group?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "platby_category_group" */
export type Platby_Category_Group_Variance_Order_By = {
  pcg_id?: InputMaybe<Order_By>;
  pcg_id_category?: InputMaybe<Order_By>;
  pcg_id_group?: InputMaybe<Order_By>;
};

/** input type for incrementing numeric columns in table "platby_category" */
export type Platby_Category_Inc_Input = {
  pc_amount?: InputMaybe<Scalars['numeric']>;
  pc_id?: InputMaybe<Scalars['bigint']>;
  pc_symbol?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "platby_category" */
export type Platby_Category_Insert_Input = {
  pc_amount?: InputMaybe<Scalars['numeric']>;
  pc_archive?: InputMaybe<Scalars['Boolean']>;
  pc_date_due?: InputMaybe<Scalars['date']>;
  pc_id?: InputMaybe<Scalars['bigint']>;
  pc_name?: InputMaybe<Scalars['String']>;
  pc_symbol?: InputMaybe<Scalars['bigint']>;
  pc_use_base?: InputMaybe<Scalars['Boolean']>;
  pc_use_prefix?: InputMaybe<Scalars['Boolean']>;
  pc_valid_from?: InputMaybe<Scalars['date']>;
  pc_valid_to?: InputMaybe<Scalars['date']>;
  pc_visible?: InputMaybe<Scalars['Boolean']>;
  platby_category_groups?: InputMaybe<Platby_Category_Group_Arr_Rel_Insert_Input>;
  platby_items?: InputMaybe<Platby_Item_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Platby_Category_Max_Fields = {
  __typename?: 'platby_category_max_fields';
  pc_amount?: Maybe<Scalars['numeric']>;
  pc_date_due?: Maybe<Scalars['date']>;
  pc_id?: Maybe<Scalars['bigint']>;
  pc_name?: Maybe<Scalars['String']>;
  pc_symbol?: Maybe<Scalars['bigint']>;
  pc_valid_from?: Maybe<Scalars['date']>;
  pc_valid_to?: Maybe<Scalars['date']>;
};

/** aggregate min on columns */
export type Platby_Category_Min_Fields = {
  __typename?: 'platby_category_min_fields';
  pc_amount?: Maybe<Scalars['numeric']>;
  pc_date_due?: Maybe<Scalars['date']>;
  pc_id?: Maybe<Scalars['bigint']>;
  pc_name?: Maybe<Scalars['String']>;
  pc_symbol?: Maybe<Scalars['bigint']>;
  pc_valid_from?: Maybe<Scalars['date']>;
  pc_valid_to?: Maybe<Scalars['date']>;
};

/** response of any mutation on the table "platby_category" */
export type Platby_Category_Mutation_Response = {
  __typename?: 'platby_category_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Category>;
};

/** input type for inserting object relation for remote table "platby_category" */
export type Platby_Category_Obj_Rel_Insert_Input = {
  data: Platby_Category_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Category_On_Conflict>;
};

/** on conflict condition type for table "platby_category" */
export type Platby_Category_On_Conflict = {
  constraint: Platby_Category_Constraint;
  update_columns?: Array<Platby_Category_Update_Column>;
  where?: InputMaybe<Platby_Category_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_category". */
export type Platby_Category_Order_By = {
  pc_amount?: InputMaybe<Order_By>;
  pc_archive?: InputMaybe<Order_By>;
  pc_date_due?: InputMaybe<Order_By>;
  pc_id?: InputMaybe<Order_By>;
  pc_name?: InputMaybe<Order_By>;
  pc_symbol?: InputMaybe<Order_By>;
  pc_use_base?: InputMaybe<Order_By>;
  pc_use_prefix?: InputMaybe<Order_By>;
  pc_valid_from?: InputMaybe<Order_By>;
  pc_valid_to?: InputMaybe<Order_By>;
  pc_visible?: InputMaybe<Order_By>;
  platby_category_groups_aggregate?: InputMaybe<Platby_Category_Group_Aggregate_Order_By>;
  platby_items_aggregate?: InputMaybe<Platby_Item_Aggregate_Order_By>;
};

/** primary key columns input for table: platby_category */
export type Platby_Category_Pk_Columns_Input = {
  pc_id: Scalars['bigint'];
};

/** select columns of table "platby_category" */
export enum Platby_Category_Select_Column {
  /** column name */
  PcAmount = 'pc_amount',
  /** column name */
  PcArchive = 'pc_archive',
  /** column name */
  PcDateDue = 'pc_date_due',
  /** column name */
  PcId = 'pc_id',
  /** column name */
  PcName = 'pc_name',
  /** column name */
  PcSymbol = 'pc_symbol',
  /** column name */
  PcUseBase = 'pc_use_base',
  /** column name */
  PcUsePrefix = 'pc_use_prefix',
  /** column name */
  PcValidFrom = 'pc_valid_from',
  /** column name */
  PcValidTo = 'pc_valid_to',
  /** column name */
  PcVisible = 'pc_visible'
}

/** input type for updating data in table "platby_category" */
export type Platby_Category_Set_Input = {
  pc_amount?: InputMaybe<Scalars['numeric']>;
  pc_archive?: InputMaybe<Scalars['Boolean']>;
  pc_date_due?: InputMaybe<Scalars['date']>;
  pc_id?: InputMaybe<Scalars['bigint']>;
  pc_name?: InputMaybe<Scalars['String']>;
  pc_symbol?: InputMaybe<Scalars['bigint']>;
  pc_use_base?: InputMaybe<Scalars['Boolean']>;
  pc_use_prefix?: InputMaybe<Scalars['Boolean']>;
  pc_valid_from?: InputMaybe<Scalars['date']>;
  pc_valid_to?: InputMaybe<Scalars['date']>;
  pc_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Platby_Category_Stddev_Fields = {
  __typename?: 'platby_category_stddev_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Platby_Category_Stddev_Pop_Fields = {
  __typename?: 'platby_category_stddev_pop_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Platby_Category_Stddev_Samp_Fields = {
  __typename?: 'platby_category_stddev_samp_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Platby_Category_Sum_Fields = {
  __typename?: 'platby_category_sum_fields';
  pc_amount?: Maybe<Scalars['numeric']>;
  pc_id?: Maybe<Scalars['bigint']>;
  pc_symbol?: Maybe<Scalars['bigint']>;
};

/** update columns of table "platby_category" */
export enum Platby_Category_Update_Column {
  /** column name */
  PcAmount = 'pc_amount',
  /** column name */
  PcArchive = 'pc_archive',
  /** column name */
  PcDateDue = 'pc_date_due',
  /** column name */
  PcId = 'pc_id',
  /** column name */
  PcName = 'pc_name',
  /** column name */
  PcSymbol = 'pc_symbol',
  /** column name */
  PcUseBase = 'pc_use_base',
  /** column name */
  PcUsePrefix = 'pc_use_prefix',
  /** column name */
  PcValidFrom = 'pc_valid_from',
  /** column name */
  PcValidTo = 'pc_valid_to',
  /** column name */
  PcVisible = 'pc_visible'
}

/** aggregate var_pop on columns */
export type Platby_Category_Var_Pop_Fields = {
  __typename?: 'platby_category_var_pop_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Platby_Category_Var_Samp_Fields = {
  __typename?: 'platby_category_var_samp_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Platby_Category_Variance_Fields = {
  __typename?: 'platby_category_variance_fields';
  pc_amount?: Maybe<Scalars['Float']>;
  pc_id?: Maybe<Scalars['Float']>;
  pc_symbol?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "platby_group" */
export type Platby_Group = {
  __typename?: 'platby_group';
  pg_base: Scalars['bigint'];
  pg_description: Scalars['String'];
  pg_id: Scalars['bigint'];
  pg_name: Scalars['String'];
  pg_type: Scalars['numeric'];
  /** An array relationship */
  platby_category_groups: Array<Platby_Category_Group>;
  /** An aggregate relationship */
  platby_category_groups_aggregate: Platby_Category_Group_Aggregate;
  /** An array relationship */
  platby_group_skupinas: Array<Platby_Group_Skupina>;
  /** An aggregate relationship */
  platby_group_skupinas_aggregate: Platby_Group_Skupina_Aggregate;
};


/** columns and relationships of "platby_group" */
export type Platby_GroupPlatby_Category_GroupsArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


/** columns and relationships of "platby_group" */
export type Platby_GroupPlatby_Category_Groups_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


/** columns and relationships of "platby_group" */
export type Platby_GroupPlatby_Group_SkupinasArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


/** columns and relationships of "platby_group" */
export type Platby_GroupPlatby_Group_Skupinas_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};

/** aggregated selection of "platby_group" */
export type Platby_Group_Aggregate = {
  __typename?: 'platby_group_aggregate';
  aggregate?: Maybe<Platby_Group_Aggregate_Fields>;
  nodes: Array<Platby_Group>;
};

/** aggregate fields of "platby_group" */
export type Platby_Group_Aggregate_Fields = {
  __typename?: 'platby_group_aggregate_fields';
  avg?: Maybe<Platby_Group_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Group_Max_Fields>;
  min?: Maybe<Platby_Group_Min_Fields>;
  stddev?: Maybe<Platby_Group_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Group_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Group_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Group_Sum_Fields>;
  var_pop?: Maybe<Platby_Group_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Group_Var_Samp_Fields>;
  variance?: Maybe<Platby_Group_Variance_Fields>;
};


/** aggregate fields of "platby_group" */
export type Platby_Group_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Group_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Platby_Group_Avg_Fields = {
  __typename?: 'platby_group_avg_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "platby_group". All fields are combined with a logical 'AND'. */
export type Platby_Group_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Group_Bool_Exp>>;
  _not?: InputMaybe<Platby_Group_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Group_Bool_Exp>>;
  pg_base?: InputMaybe<Bigint_Comparison_Exp>;
  pg_description?: InputMaybe<String_Comparison_Exp>;
  pg_id?: InputMaybe<Bigint_Comparison_Exp>;
  pg_name?: InputMaybe<String_Comparison_Exp>;
  pg_type?: InputMaybe<Numeric_Comparison_Exp>;
  platby_category_groups?: InputMaybe<Platby_Category_Group_Bool_Exp>;
  platby_group_skupinas?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};

/** unique or primary key constraints on table "platby_group" */
export enum Platby_Group_Constraint {
  /** unique or primary key constraint */
  Idx_24696Primary = 'idx_24696_primary'
}

/** input type for incrementing numeric columns in table "platby_group" */
export type Platby_Group_Inc_Input = {
  pg_base?: InputMaybe<Scalars['bigint']>;
  pg_id?: InputMaybe<Scalars['bigint']>;
  pg_type?: InputMaybe<Scalars['numeric']>;
};

/** input type for inserting data into table "platby_group" */
export type Platby_Group_Insert_Input = {
  pg_base?: InputMaybe<Scalars['bigint']>;
  pg_description?: InputMaybe<Scalars['String']>;
  pg_id?: InputMaybe<Scalars['bigint']>;
  pg_name?: InputMaybe<Scalars['String']>;
  pg_type?: InputMaybe<Scalars['numeric']>;
  platby_category_groups?: InputMaybe<Platby_Category_Group_Arr_Rel_Insert_Input>;
  platby_group_skupinas?: InputMaybe<Platby_Group_Skupina_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Platby_Group_Max_Fields = {
  __typename?: 'platby_group_max_fields';
  pg_base?: Maybe<Scalars['bigint']>;
  pg_description?: Maybe<Scalars['String']>;
  pg_id?: Maybe<Scalars['bigint']>;
  pg_name?: Maybe<Scalars['String']>;
  pg_type?: Maybe<Scalars['numeric']>;
};

/** aggregate min on columns */
export type Platby_Group_Min_Fields = {
  __typename?: 'platby_group_min_fields';
  pg_base?: Maybe<Scalars['bigint']>;
  pg_description?: Maybe<Scalars['String']>;
  pg_id?: Maybe<Scalars['bigint']>;
  pg_name?: Maybe<Scalars['String']>;
  pg_type?: Maybe<Scalars['numeric']>;
};

/** response of any mutation on the table "platby_group" */
export type Platby_Group_Mutation_Response = {
  __typename?: 'platby_group_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Group>;
};

/** input type for inserting object relation for remote table "platby_group" */
export type Platby_Group_Obj_Rel_Insert_Input = {
  data: Platby_Group_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Group_On_Conflict>;
};

/** on conflict condition type for table "platby_group" */
export type Platby_Group_On_Conflict = {
  constraint: Platby_Group_Constraint;
  update_columns?: Array<Platby_Group_Update_Column>;
  where?: InputMaybe<Platby_Group_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_group". */
export type Platby_Group_Order_By = {
  pg_base?: InputMaybe<Order_By>;
  pg_description?: InputMaybe<Order_By>;
  pg_id?: InputMaybe<Order_By>;
  pg_name?: InputMaybe<Order_By>;
  pg_type?: InputMaybe<Order_By>;
  platby_category_groups_aggregate?: InputMaybe<Platby_Category_Group_Aggregate_Order_By>;
  platby_group_skupinas_aggregate?: InputMaybe<Platby_Group_Skupina_Aggregate_Order_By>;
};

/** primary key columns input for table: platby_group */
export type Platby_Group_Pk_Columns_Input = {
  pg_id: Scalars['bigint'];
};

/** select columns of table "platby_group" */
export enum Platby_Group_Select_Column {
  /** column name */
  PgBase = 'pg_base',
  /** column name */
  PgDescription = 'pg_description',
  /** column name */
  PgId = 'pg_id',
  /** column name */
  PgName = 'pg_name',
  /** column name */
  PgType = 'pg_type'
}

/** input type for updating data in table "platby_group" */
export type Platby_Group_Set_Input = {
  pg_base?: InputMaybe<Scalars['bigint']>;
  pg_description?: InputMaybe<Scalars['String']>;
  pg_id?: InputMaybe<Scalars['bigint']>;
  pg_name?: InputMaybe<Scalars['String']>;
  pg_type?: InputMaybe<Scalars['numeric']>;
};

/** columns and relationships of "platby_group_skupina" */
export type Platby_Group_Skupina = {
  __typename?: 'platby_group_skupina';
  pgs_id: Scalars['bigint'];
  pgs_id_group: Scalars['bigint'];
  pgs_id_skupina: Scalars['bigint'];
  /** An object relationship */
  platby_group: Platby_Group;
  /** An object relationship */
  skupiny: Skupiny;
};

/** aggregated selection of "platby_group_skupina" */
export type Platby_Group_Skupina_Aggregate = {
  __typename?: 'platby_group_skupina_aggregate';
  aggregate?: Maybe<Platby_Group_Skupina_Aggregate_Fields>;
  nodes: Array<Platby_Group_Skupina>;
};

/** aggregate fields of "platby_group_skupina" */
export type Platby_Group_Skupina_Aggregate_Fields = {
  __typename?: 'platby_group_skupina_aggregate_fields';
  avg?: Maybe<Platby_Group_Skupina_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Group_Skupina_Max_Fields>;
  min?: Maybe<Platby_Group_Skupina_Min_Fields>;
  stddev?: Maybe<Platby_Group_Skupina_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Group_Skupina_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Group_Skupina_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Group_Skupina_Sum_Fields>;
  var_pop?: Maybe<Platby_Group_Skupina_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Group_Skupina_Var_Samp_Fields>;
  variance?: Maybe<Platby_Group_Skupina_Variance_Fields>;
};


/** aggregate fields of "platby_group_skupina" */
export type Platby_Group_Skupina_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "platby_group_skupina" */
export type Platby_Group_Skupina_Aggregate_Order_By = {
  avg?: InputMaybe<Platby_Group_Skupina_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Platby_Group_Skupina_Max_Order_By>;
  min?: InputMaybe<Platby_Group_Skupina_Min_Order_By>;
  stddev?: InputMaybe<Platby_Group_Skupina_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Platby_Group_Skupina_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Platby_Group_Skupina_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Platby_Group_Skupina_Sum_Order_By>;
  var_pop?: InputMaybe<Platby_Group_Skupina_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Platby_Group_Skupina_Var_Samp_Order_By>;
  variance?: InputMaybe<Platby_Group_Skupina_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "platby_group_skupina" */
export type Platby_Group_Skupina_Arr_Rel_Insert_Input = {
  data: Array<Platby_Group_Skupina_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Group_Skupina_On_Conflict>;
};

/** aggregate avg on columns */
export type Platby_Group_Skupina_Avg_Fields = {
  __typename?: 'platby_group_skupina_avg_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Avg_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "platby_group_skupina". All fields are combined with a logical 'AND'. */
export type Platby_Group_Skupina_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Group_Skupina_Bool_Exp>>;
  _not?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Group_Skupina_Bool_Exp>>;
  pgs_id?: InputMaybe<Bigint_Comparison_Exp>;
  pgs_id_group?: InputMaybe<Bigint_Comparison_Exp>;
  pgs_id_skupina?: InputMaybe<Bigint_Comparison_Exp>;
  platby_group?: InputMaybe<Platby_Group_Bool_Exp>;
  skupiny?: InputMaybe<Skupiny_Bool_Exp>;
};

/** unique or primary key constraints on table "platby_group_skupina" */
export enum Platby_Group_Skupina_Constraint {
  /** unique or primary key constraint */
  Idx_24707PgsIdSkupina = 'idx_24707_pgs_id_skupina',
  /** unique or primary key constraint */
  Idx_24707Primary = 'idx_24707_primary',
  /** unique or primary key constraint */
  Idx_24708PgsIdSkupina = 'idx_24708_pgs_id_skupina'
}

/** input type for incrementing numeric columns in table "platby_group_skupina" */
export type Platby_Group_Skupina_Inc_Input = {
  pgs_id?: InputMaybe<Scalars['bigint']>;
  pgs_id_group?: InputMaybe<Scalars['bigint']>;
  pgs_id_skupina?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "platby_group_skupina" */
export type Platby_Group_Skupina_Insert_Input = {
  pgs_id?: InputMaybe<Scalars['bigint']>;
  pgs_id_group?: InputMaybe<Scalars['bigint']>;
  pgs_id_skupina?: InputMaybe<Scalars['bigint']>;
  platby_group?: InputMaybe<Platby_Group_Obj_Rel_Insert_Input>;
  skupiny?: InputMaybe<Skupiny_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Platby_Group_Skupina_Max_Fields = {
  __typename?: 'platby_group_skupina_max_fields';
  pgs_id?: Maybe<Scalars['bigint']>;
  pgs_id_group?: Maybe<Scalars['bigint']>;
  pgs_id_skupina?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Max_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Platby_Group_Skupina_Min_Fields = {
  __typename?: 'platby_group_skupina_min_fields';
  pgs_id?: Maybe<Scalars['bigint']>;
  pgs_id_group?: Maybe<Scalars['bigint']>;
  pgs_id_skupina?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Min_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "platby_group_skupina" */
export type Platby_Group_Skupina_Mutation_Response = {
  __typename?: 'platby_group_skupina_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Group_Skupina>;
};

/** on conflict condition type for table "platby_group_skupina" */
export type Platby_Group_Skupina_On_Conflict = {
  constraint: Platby_Group_Skupina_Constraint;
  update_columns?: Array<Platby_Group_Skupina_Update_Column>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_group_skupina". */
export type Platby_Group_Skupina_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
  platby_group?: InputMaybe<Platby_Group_Order_By>;
  skupiny?: InputMaybe<Skupiny_Order_By>;
};

/** primary key columns input for table: platby_group_skupina */
export type Platby_Group_Skupina_Pk_Columns_Input = {
  pgs_id: Scalars['bigint'];
};

/** select columns of table "platby_group_skupina" */
export enum Platby_Group_Skupina_Select_Column {
  /** column name */
  PgsId = 'pgs_id',
  /** column name */
  PgsIdGroup = 'pgs_id_group',
  /** column name */
  PgsIdSkupina = 'pgs_id_skupina'
}

/** input type for updating data in table "platby_group_skupina" */
export type Platby_Group_Skupina_Set_Input = {
  pgs_id?: InputMaybe<Scalars['bigint']>;
  pgs_id_group?: InputMaybe<Scalars['bigint']>;
  pgs_id_skupina?: InputMaybe<Scalars['bigint']>;
};

/** aggregate stddev on columns */
export type Platby_Group_Skupina_Stddev_Fields = {
  __typename?: 'platby_group_skupina_stddev_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Stddev_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Platby_Group_Skupina_Stddev_Pop_Fields = {
  __typename?: 'platby_group_skupina_stddev_pop_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Stddev_Pop_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Platby_Group_Skupina_Stddev_Samp_Fields = {
  __typename?: 'platby_group_skupina_stddev_samp_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Stddev_Samp_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Platby_Group_Skupina_Sum_Fields = {
  __typename?: 'platby_group_skupina_sum_fields';
  pgs_id?: Maybe<Scalars['bigint']>;
  pgs_id_group?: Maybe<Scalars['bigint']>;
  pgs_id_skupina?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Sum_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** update columns of table "platby_group_skupina" */
export enum Platby_Group_Skupina_Update_Column {
  /** column name */
  PgsId = 'pgs_id',
  /** column name */
  PgsIdGroup = 'pgs_id_group',
  /** column name */
  PgsIdSkupina = 'pgs_id_skupina'
}

/** aggregate var_pop on columns */
export type Platby_Group_Skupina_Var_Pop_Fields = {
  __typename?: 'platby_group_skupina_var_pop_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Var_Pop_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Platby_Group_Skupina_Var_Samp_Fields = {
  __typename?: 'platby_group_skupina_var_samp_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Var_Samp_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Platby_Group_Skupina_Variance_Fields = {
  __typename?: 'platby_group_skupina_variance_fields';
  pgs_id?: Maybe<Scalars['Float']>;
  pgs_id_group?: Maybe<Scalars['Float']>;
  pgs_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "platby_group_skupina" */
export type Platby_Group_Skupina_Variance_Order_By = {
  pgs_id?: InputMaybe<Order_By>;
  pgs_id_group?: InputMaybe<Order_By>;
  pgs_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev on columns */
export type Platby_Group_Stddev_Fields = {
  __typename?: 'platby_group_stddev_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Platby_Group_Stddev_Pop_Fields = {
  __typename?: 'platby_group_stddev_pop_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Platby_Group_Stddev_Samp_Fields = {
  __typename?: 'platby_group_stddev_samp_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Platby_Group_Sum_Fields = {
  __typename?: 'platby_group_sum_fields';
  pg_base?: Maybe<Scalars['bigint']>;
  pg_id?: Maybe<Scalars['bigint']>;
  pg_type?: Maybe<Scalars['numeric']>;
};

/** update columns of table "platby_group" */
export enum Platby_Group_Update_Column {
  /** column name */
  PgBase = 'pg_base',
  /** column name */
  PgDescription = 'pg_description',
  /** column name */
  PgId = 'pg_id',
  /** column name */
  PgName = 'pg_name',
  /** column name */
  PgType = 'pg_type'
}

/** aggregate var_pop on columns */
export type Platby_Group_Var_Pop_Fields = {
  __typename?: 'platby_group_var_pop_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Platby_Group_Var_Samp_Fields = {
  __typename?: 'platby_group_var_samp_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Platby_Group_Variance_Fields = {
  __typename?: 'platby_group_variance_fields';
  pg_base?: Maybe<Scalars['Float']>;
  pg_id?: Maybe<Scalars['Float']>;
  pg_type?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "platby_item" */
export type Platby_Item = {
  __typename?: 'platby_item';
  pi_amount: Scalars['numeric'];
  pi_date: Scalars['date'];
  pi_id: Scalars['bigint'];
  pi_id_category: Scalars['bigint'];
  pi_id_raw?: Maybe<Scalars['bigint']>;
  pi_id_user?: Maybe<Scalars['bigint']>;
  pi_prefix: Scalars['Int'];
  /** An object relationship */
  platby_category: Platby_Category;
  /** An object relationship */
  platby_raw?: Maybe<Platby_Raw>;
  /** An object relationship */
  user?: Maybe<Users>;
};

/** aggregated selection of "platby_item" */
export type Platby_Item_Aggregate = {
  __typename?: 'platby_item_aggregate';
  aggregate?: Maybe<Platby_Item_Aggregate_Fields>;
  nodes: Array<Platby_Item>;
};

/** aggregate fields of "platby_item" */
export type Platby_Item_Aggregate_Fields = {
  __typename?: 'platby_item_aggregate_fields';
  avg?: Maybe<Platby_Item_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Item_Max_Fields>;
  min?: Maybe<Platby_Item_Min_Fields>;
  stddev?: Maybe<Platby_Item_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Item_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Item_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Item_Sum_Fields>;
  var_pop?: Maybe<Platby_Item_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Item_Var_Samp_Fields>;
  variance?: Maybe<Platby_Item_Variance_Fields>;
};


/** aggregate fields of "platby_item" */
export type Platby_Item_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Item_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "platby_item" */
export type Platby_Item_Aggregate_Order_By = {
  avg?: InputMaybe<Platby_Item_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Platby_Item_Max_Order_By>;
  min?: InputMaybe<Platby_Item_Min_Order_By>;
  stddev?: InputMaybe<Platby_Item_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Platby_Item_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Platby_Item_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Platby_Item_Sum_Order_By>;
  var_pop?: InputMaybe<Platby_Item_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Platby_Item_Var_Samp_Order_By>;
  variance?: InputMaybe<Platby_Item_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "platby_item" */
export type Platby_Item_Arr_Rel_Insert_Input = {
  data: Array<Platby_Item_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Item_On_Conflict>;
};

/** aggregate avg on columns */
export type Platby_Item_Avg_Fields = {
  __typename?: 'platby_item_avg_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "platby_item" */
export type Platby_Item_Avg_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "platby_item". All fields are combined with a logical 'AND'. */
export type Platby_Item_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Item_Bool_Exp>>;
  _not?: InputMaybe<Platby_Item_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Item_Bool_Exp>>;
  pi_amount?: InputMaybe<Numeric_Comparison_Exp>;
  pi_date?: InputMaybe<Date_Comparison_Exp>;
  pi_id?: InputMaybe<Bigint_Comparison_Exp>;
  pi_id_category?: InputMaybe<Bigint_Comparison_Exp>;
  pi_id_raw?: InputMaybe<Bigint_Comparison_Exp>;
  pi_id_user?: InputMaybe<Bigint_Comparison_Exp>;
  pi_prefix?: InputMaybe<Int_Comparison_Exp>;
  platby_category?: InputMaybe<Platby_Category_Bool_Exp>;
  platby_raw?: InputMaybe<Platby_Raw_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "platby_item" */
export enum Platby_Item_Constraint {
  /** unique or primary key constraint */
  Idx_24713PiIdRaw = 'idx_24713_pi_id_raw',
  /** unique or primary key constraint */
  Idx_24713Primary = 'idx_24713_primary'
}

/** input type for incrementing numeric columns in table "platby_item" */
export type Platby_Item_Inc_Input = {
  pi_amount?: InputMaybe<Scalars['numeric']>;
  pi_id?: InputMaybe<Scalars['bigint']>;
  pi_id_category?: InputMaybe<Scalars['bigint']>;
  pi_id_raw?: InputMaybe<Scalars['bigint']>;
  pi_id_user?: InputMaybe<Scalars['bigint']>;
  pi_prefix?: InputMaybe<Scalars['Int']>;
};

/** input type for inserting data into table "platby_item" */
export type Platby_Item_Insert_Input = {
  pi_amount?: InputMaybe<Scalars['numeric']>;
  pi_date?: InputMaybe<Scalars['date']>;
  pi_id?: InputMaybe<Scalars['bigint']>;
  pi_id_category?: InputMaybe<Scalars['bigint']>;
  pi_id_raw?: InputMaybe<Scalars['bigint']>;
  pi_id_user?: InputMaybe<Scalars['bigint']>;
  pi_prefix?: InputMaybe<Scalars['Int']>;
  platby_category?: InputMaybe<Platby_Category_Obj_Rel_Insert_Input>;
  platby_raw?: InputMaybe<Platby_Raw_Obj_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Platby_Item_Max_Fields = {
  __typename?: 'platby_item_max_fields';
  pi_amount?: Maybe<Scalars['numeric']>;
  pi_date?: Maybe<Scalars['date']>;
  pi_id?: Maybe<Scalars['bigint']>;
  pi_id_category?: Maybe<Scalars['bigint']>;
  pi_id_raw?: Maybe<Scalars['bigint']>;
  pi_id_user?: Maybe<Scalars['bigint']>;
  pi_prefix?: Maybe<Scalars['Int']>;
};

/** order by max() on columns of table "platby_item" */
export type Platby_Item_Max_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_date?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Platby_Item_Min_Fields = {
  __typename?: 'platby_item_min_fields';
  pi_amount?: Maybe<Scalars['numeric']>;
  pi_date?: Maybe<Scalars['date']>;
  pi_id?: Maybe<Scalars['bigint']>;
  pi_id_category?: Maybe<Scalars['bigint']>;
  pi_id_raw?: Maybe<Scalars['bigint']>;
  pi_id_user?: Maybe<Scalars['bigint']>;
  pi_prefix?: Maybe<Scalars['Int']>;
};

/** order by min() on columns of table "platby_item" */
export type Platby_Item_Min_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_date?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "platby_item" */
export type Platby_Item_Mutation_Response = {
  __typename?: 'platby_item_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Item>;
};

/** on conflict condition type for table "platby_item" */
export type Platby_Item_On_Conflict = {
  constraint: Platby_Item_Constraint;
  update_columns?: Array<Platby_Item_Update_Column>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_item". */
export type Platby_Item_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_date?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
  platby_category?: InputMaybe<Platby_Category_Order_By>;
  platby_raw?: InputMaybe<Platby_Raw_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: platby_item */
export type Platby_Item_Pk_Columns_Input = {
  pi_id: Scalars['bigint'];
};

/** select columns of table "platby_item" */
export enum Platby_Item_Select_Column {
  /** column name */
  PiAmount = 'pi_amount',
  /** column name */
  PiDate = 'pi_date',
  /** column name */
  PiId = 'pi_id',
  /** column name */
  PiIdCategory = 'pi_id_category',
  /** column name */
  PiIdRaw = 'pi_id_raw',
  /** column name */
  PiIdUser = 'pi_id_user',
  /** column name */
  PiPrefix = 'pi_prefix'
}

/** input type for updating data in table "platby_item" */
export type Platby_Item_Set_Input = {
  pi_amount?: InputMaybe<Scalars['numeric']>;
  pi_date?: InputMaybe<Scalars['date']>;
  pi_id?: InputMaybe<Scalars['bigint']>;
  pi_id_category?: InputMaybe<Scalars['bigint']>;
  pi_id_raw?: InputMaybe<Scalars['bigint']>;
  pi_id_user?: InputMaybe<Scalars['bigint']>;
  pi_prefix?: InputMaybe<Scalars['Int']>;
};

/** aggregate stddev on columns */
export type Platby_Item_Stddev_Fields = {
  __typename?: 'platby_item_stddev_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "platby_item" */
export type Platby_Item_Stddev_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Platby_Item_Stddev_Pop_Fields = {
  __typename?: 'platby_item_stddev_pop_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "platby_item" */
export type Platby_Item_Stddev_Pop_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Platby_Item_Stddev_Samp_Fields = {
  __typename?: 'platby_item_stddev_samp_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "platby_item" */
export type Platby_Item_Stddev_Samp_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Platby_Item_Sum_Fields = {
  __typename?: 'platby_item_sum_fields';
  pi_amount?: Maybe<Scalars['numeric']>;
  pi_id?: Maybe<Scalars['bigint']>;
  pi_id_category?: Maybe<Scalars['bigint']>;
  pi_id_raw?: Maybe<Scalars['bigint']>;
  pi_id_user?: Maybe<Scalars['bigint']>;
  pi_prefix?: Maybe<Scalars['Int']>;
};

/** order by sum() on columns of table "platby_item" */
export type Platby_Item_Sum_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** update columns of table "platby_item" */
export enum Platby_Item_Update_Column {
  /** column name */
  PiAmount = 'pi_amount',
  /** column name */
  PiDate = 'pi_date',
  /** column name */
  PiId = 'pi_id',
  /** column name */
  PiIdCategory = 'pi_id_category',
  /** column name */
  PiIdRaw = 'pi_id_raw',
  /** column name */
  PiIdUser = 'pi_id_user',
  /** column name */
  PiPrefix = 'pi_prefix'
}

/** aggregate var_pop on columns */
export type Platby_Item_Var_Pop_Fields = {
  __typename?: 'platby_item_var_pop_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "platby_item" */
export type Platby_Item_Var_Pop_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Platby_Item_Var_Samp_Fields = {
  __typename?: 'platby_item_var_samp_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "platby_item" */
export type Platby_Item_Var_Samp_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Platby_Item_Variance_Fields = {
  __typename?: 'platby_item_variance_fields';
  pi_amount?: Maybe<Scalars['Float']>;
  pi_id?: Maybe<Scalars['Float']>;
  pi_id_category?: Maybe<Scalars['Float']>;
  pi_id_raw?: Maybe<Scalars['Float']>;
  pi_id_user?: Maybe<Scalars['Float']>;
  pi_prefix?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "platby_item" */
export type Platby_Item_Variance_Order_By = {
  pi_amount?: InputMaybe<Order_By>;
  pi_id?: InputMaybe<Order_By>;
  pi_id_category?: InputMaybe<Order_By>;
  pi_id_raw?: InputMaybe<Order_By>;
  pi_id_user?: InputMaybe<Order_By>;
  pi_prefix?: InputMaybe<Order_By>;
};

/** columns and relationships of "platby_raw" */
export type Platby_Raw = {
  __typename?: 'platby_raw';
  /** An array relationship */
  platby_items: Array<Platby_Item>;
  /** An aggregate relationship */
  platby_items_aggregate: Platby_Item_Aggregate;
  pr_discarded: Scalars['Boolean'];
  pr_hash: Scalars['String'];
  pr_id: Scalars['bigint'];
  pr_raw: Scalars['bytea'];
  pr_sorted: Scalars['Boolean'];
};


/** columns and relationships of "platby_raw" */
export type Platby_RawPlatby_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


/** columns and relationships of "platby_raw" */
export type Platby_RawPlatby_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};

/** aggregated selection of "platby_raw" */
export type Platby_Raw_Aggregate = {
  __typename?: 'platby_raw_aggregate';
  aggregate?: Maybe<Platby_Raw_Aggregate_Fields>;
  nodes: Array<Platby_Raw>;
};

/** aggregate fields of "platby_raw" */
export type Platby_Raw_Aggregate_Fields = {
  __typename?: 'platby_raw_aggregate_fields';
  avg?: Maybe<Platby_Raw_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Platby_Raw_Max_Fields>;
  min?: Maybe<Platby_Raw_Min_Fields>;
  stddev?: Maybe<Platby_Raw_Stddev_Fields>;
  stddev_pop?: Maybe<Platby_Raw_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Platby_Raw_Stddev_Samp_Fields>;
  sum?: Maybe<Platby_Raw_Sum_Fields>;
  var_pop?: Maybe<Platby_Raw_Var_Pop_Fields>;
  var_samp?: Maybe<Platby_Raw_Var_Samp_Fields>;
  variance?: Maybe<Platby_Raw_Variance_Fields>;
};


/** aggregate fields of "platby_raw" */
export type Platby_Raw_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Platby_Raw_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Platby_Raw_Avg_Fields = {
  __typename?: 'platby_raw_avg_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "platby_raw". All fields are combined with a logical 'AND'. */
export type Platby_Raw_Bool_Exp = {
  _and?: InputMaybe<Array<Platby_Raw_Bool_Exp>>;
  _not?: InputMaybe<Platby_Raw_Bool_Exp>;
  _or?: InputMaybe<Array<Platby_Raw_Bool_Exp>>;
  platby_items?: InputMaybe<Platby_Item_Bool_Exp>;
  pr_discarded?: InputMaybe<Boolean_Comparison_Exp>;
  pr_hash?: InputMaybe<String_Comparison_Exp>;
  pr_id?: InputMaybe<Bigint_Comparison_Exp>;
  pr_raw?: InputMaybe<Bytea_Comparison_Exp>;
  pr_sorted?: InputMaybe<Boolean_Comparison_Exp>;
};

/** unique or primary key constraints on table "platby_raw" */
export enum Platby_Raw_Constraint {
  /** unique or primary key constraint */
  Idx_24720PrHash = 'idx_24720_pr_hash',
  /** unique or primary key constraint */
  Idx_24720Primary = 'idx_24720_primary'
}

/** input type for incrementing numeric columns in table "platby_raw" */
export type Platby_Raw_Inc_Input = {
  pr_id?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "platby_raw" */
export type Platby_Raw_Insert_Input = {
  platby_items?: InputMaybe<Platby_Item_Arr_Rel_Insert_Input>;
  pr_discarded?: InputMaybe<Scalars['Boolean']>;
  pr_hash?: InputMaybe<Scalars['String']>;
  pr_id?: InputMaybe<Scalars['bigint']>;
  pr_raw?: InputMaybe<Scalars['bytea']>;
  pr_sorted?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate max on columns */
export type Platby_Raw_Max_Fields = {
  __typename?: 'platby_raw_max_fields';
  pr_hash?: Maybe<Scalars['String']>;
  pr_id?: Maybe<Scalars['bigint']>;
};

/** aggregate min on columns */
export type Platby_Raw_Min_Fields = {
  __typename?: 'platby_raw_min_fields';
  pr_hash?: Maybe<Scalars['String']>;
  pr_id?: Maybe<Scalars['bigint']>;
};

/** response of any mutation on the table "platby_raw" */
export type Platby_Raw_Mutation_Response = {
  __typename?: 'platby_raw_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Platby_Raw>;
};

/** input type for inserting object relation for remote table "platby_raw" */
export type Platby_Raw_Obj_Rel_Insert_Input = {
  data: Platby_Raw_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Platby_Raw_On_Conflict>;
};

/** on conflict condition type for table "platby_raw" */
export type Platby_Raw_On_Conflict = {
  constraint: Platby_Raw_Constraint;
  update_columns?: Array<Platby_Raw_Update_Column>;
  where?: InputMaybe<Platby_Raw_Bool_Exp>;
};

/** Ordering options when selecting data from "platby_raw". */
export type Platby_Raw_Order_By = {
  platby_items_aggregate?: InputMaybe<Platby_Item_Aggregate_Order_By>;
  pr_discarded?: InputMaybe<Order_By>;
  pr_hash?: InputMaybe<Order_By>;
  pr_id?: InputMaybe<Order_By>;
  pr_raw?: InputMaybe<Order_By>;
  pr_sorted?: InputMaybe<Order_By>;
};

/** primary key columns input for table: platby_raw */
export type Platby_Raw_Pk_Columns_Input = {
  pr_id: Scalars['bigint'];
};

/** select columns of table "platby_raw" */
export enum Platby_Raw_Select_Column {
  /** column name */
  PrDiscarded = 'pr_discarded',
  /** column name */
  PrHash = 'pr_hash',
  /** column name */
  PrId = 'pr_id',
  /** column name */
  PrRaw = 'pr_raw',
  /** column name */
  PrSorted = 'pr_sorted'
}

/** input type for updating data in table "platby_raw" */
export type Platby_Raw_Set_Input = {
  pr_discarded?: InputMaybe<Scalars['Boolean']>;
  pr_hash?: InputMaybe<Scalars['String']>;
  pr_id?: InputMaybe<Scalars['bigint']>;
  pr_raw?: InputMaybe<Scalars['bytea']>;
  pr_sorted?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Platby_Raw_Stddev_Fields = {
  __typename?: 'platby_raw_stddev_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Platby_Raw_Stddev_Pop_Fields = {
  __typename?: 'platby_raw_stddev_pop_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Platby_Raw_Stddev_Samp_Fields = {
  __typename?: 'platby_raw_stddev_samp_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Platby_Raw_Sum_Fields = {
  __typename?: 'platby_raw_sum_fields';
  pr_id?: Maybe<Scalars['bigint']>;
};

/** update columns of table "platby_raw" */
export enum Platby_Raw_Update_Column {
  /** column name */
  PrDiscarded = 'pr_discarded',
  /** column name */
  PrHash = 'pr_hash',
  /** column name */
  PrId = 'pr_id',
  /** column name */
  PrRaw = 'pr_raw',
  /** column name */
  PrSorted = 'pr_sorted'
}

/** aggregate var_pop on columns */
export type Platby_Raw_Var_Pop_Fields = {
  __typename?: 'platby_raw_var_pop_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Platby_Raw_Var_Samp_Fields = {
  __typename?: 'platby_raw_var_samp_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Platby_Raw_Variance_Fields = {
  __typename?: 'platby_raw_variance_fields';
  pr_id?: Maybe<Scalars['Float']>;
};

export type Query_Root = {
  __typename?: 'query_root';
  /** fetch data from the table: "akce" */
  akce: Array<Akce>;
  /** fetch aggregated fields from the table: "akce" */
  akce_aggregate: Akce_Aggregate;
  /** fetch data from the table: "akce" using primary key columns */
  akce_by_pk?: Maybe<Akce>;
  /** fetch data from the table: "akce_item" */
  akce_item: Array<Akce_Item>;
  /** fetch aggregated fields from the table: "akce_item" */
  akce_item_aggregate: Akce_Item_Aggregate;
  /** fetch data from the table: "akce_item" using primary key columns */
  akce_item_by_pk?: Maybe<Akce_Item>;
  /** fetch data from the table: "aktuality" */
  aktuality: Array<Aktuality>;
  /** fetch data from the table: "aktuality_admin" */
  aktuality_admin: Array<Aktuality_Admin>;
  /** fetch aggregated fields from the table: "aktuality_admin" */
  aktuality_admin_aggregate: Aktuality_Admin_Aggregate;
  /** fetch aggregated fields from the table: "aktuality" */
  aktuality_aggregate: Aktuality_Aggregate;
  /** fetch data from the table: "aktuality" using primary key columns */
  aktuality_by_pk?: Maybe<Aktuality>;
  /** fetch data from the table: "dokumenty" */
  dokumenty: Array<Dokumenty>;
  /** fetch aggregated fields from the table: "dokumenty" */
  dokumenty_aggregate: Dokumenty_Aggregate;
  /** fetch data from the table: "dokumenty" using primary key columns */
  dokumenty_by_pk?: Maybe<Dokumenty>;
  /** fetch data from the table: "galerie_dir" */
  galerie_dir: Array<Galerie_Dir>;
  /** fetch aggregated fields from the table: "galerie_dir" */
  galerie_dir_aggregate: Galerie_Dir_Aggregate;
  /** fetch data from the table: "galerie_dir" using primary key columns */
  galerie_dir_by_pk?: Maybe<Galerie_Dir>;
  /** fetch data from the table: "galerie_foto" */
  galerie_foto: Array<Galerie_Foto>;
  /** fetch aggregated fields from the table: "galerie_foto" */
  galerie_foto_aggregate: Galerie_Foto_Aggregate;
  /** fetch data from the table: "galerie_foto" using primary key columns */
  galerie_foto_by_pk?: Maybe<Galerie_Foto>;
  /** fetch data from the table: "nabidka" */
  nabidka: Array<Nabidka>;
  /** fetch data from the table: "nabidka_admin" */
  nabidka_admin: Array<Nabidka_Admin>;
  /** fetch aggregated fields from the table: "nabidka_admin" */
  nabidka_admin_aggregate: Nabidka_Admin_Aggregate;
  /** fetch aggregated fields from the table: "nabidka" */
  nabidka_aggregate: Nabidka_Aggregate;
  /** fetch data from the table: "nabidka" using primary key columns */
  nabidka_by_pk?: Maybe<Nabidka>;
  /** fetch data from the table: "nabidka_item" */
  nabidka_item: Array<Nabidka_Item>;
  /** fetch aggregated fields from the table: "nabidka_item" */
  nabidka_item_aggregate: Nabidka_Item_Aggregate;
  /** fetch data from the table: "nabidka_item" using primary key columns */
  nabidka_item_by_pk?: Maybe<Nabidka_Item>;
  /** fetch data from the table: "parameters" */
  parameters: Array<Parameters>;
  /** fetch aggregated fields from the table: "parameters" */
  parameters_aggregate: Parameters_Aggregate;
  /** fetch data from the table: "parameters" using primary key columns */
  parameters_by_pk?: Maybe<Parameters>;
  /** fetch data from the table: "pary" */
  pary: Array<Pary>;
  /** fetch aggregated fields from the table: "pary" */
  pary_aggregate: Pary_Aggregate;
  /** fetch data from the table: "pary" using primary key columns */
  pary_by_pk?: Maybe<Pary>;
  /** fetch data from the table: "pary_navrh" */
  pary_navrh: Array<Pary_Navrh>;
  /** fetch aggregated fields from the table: "pary_navrh" */
  pary_navrh_aggregate: Pary_Navrh_Aggregate;
  /** fetch data from the table: "pary_navrh" using primary key columns */
  pary_navrh_by_pk?: Maybe<Pary_Navrh>;
  /** fetch data from the table: "permissions" */
  permissions: Array<Permissions>;
  /** fetch aggregated fields from the table: "permissions" */
  permissions_aggregate: Permissions_Aggregate;
  /** fetch data from the table: "permissions" using primary key columns */
  permissions_by_pk?: Maybe<Permissions>;
  /** fetch data from the table: "platby_category" */
  platby_category: Array<Platby_Category>;
  /** fetch aggregated fields from the table: "platby_category" */
  platby_category_aggregate: Platby_Category_Aggregate;
  /** fetch data from the table: "platby_category" using primary key columns */
  platby_category_by_pk?: Maybe<Platby_Category>;
  /** fetch data from the table: "platby_category_group" */
  platby_category_group: Array<Platby_Category_Group>;
  /** fetch aggregated fields from the table: "platby_category_group" */
  platby_category_group_aggregate: Platby_Category_Group_Aggregate;
  /** fetch data from the table: "platby_category_group" using primary key columns */
  platby_category_group_by_pk?: Maybe<Platby_Category_Group>;
  /** fetch data from the table: "platby_group" */
  platby_group: Array<Platby_Group>;
  /** fetch aggregated fields from the table: "platby_group" */
  platby_group_aggregate: Platby_Group_Aggregate;
  /** fetch data from the table: "platby_group" using primary key columns */
  platby_group_by_pk?: Maybe<Platby_Group>;
  /** fetch data from the table: "platby_group_skupina" */
  platby_group_skupina: Array<Platby_Group_Skupina>;
  /** fetch aggregated fields from the table: "platby_group_skupina" */
  platby_group_skupina_aggregate: Platby_Group_Skupina_Aggregate;
  /** fetch data from the table: "platby_group_skupina" using primary key columns */
  platby_group_skupina_by_pk?: Maybe<Platby_Group_Skupina>;
  /** fetch data from the table: "platby_item" */
  platby_item: Array<Platby_Item>;
  /** fetch aggregated fields from the table: "platby_item" */
  platby_item_aggregate: Platby_Item_Aggregate;
  /** fetch data from the table: "platby_item" using primary key columns */
  platby_item_by_pk?: Maybe<Platby_Item>;
  /** fetch data from the table: "platby_raw" */
  platby_raw: Array<Platby_Raw>;
  /** fetch aggregated fields from the table: "platby_raw" */
  platby_raw_aggregate: Platby_Raw_Aggregate;
  /** fetch data from the table: "platby_raw" using primary key columns */
  platby_raw_by_pk?: Maybe<Platby_Raw>;
  /** An array relationship */
  rozpis: Array<Rozpis>;
  /** fetch data from the table: "rozpis_admin" */
  rozpis_admin: Array<Rozpis_Admin>;
  /** fetch aggregated fields from the table: "rozpis_admin" */
  rozpis_admin_aggregate: Rozpis_Admin_Aggregate;
  /** An aggregate relationship */
  rozpis_aggregate: Rozpis_Aggregate;
  /** fetch data from the table: "rozpis" using primary key columns */
  rozpis_by_pk?: Maybe<Rozpis>;
  /** fetch data from the table: "rozpis_item" */
  rozpis_item: Array<Rozpis_Item>;
  /** fetch aggregated fields from the table: "rozpis_item" */
  rozpis_item_aggregate: Rozpis_Item_Aggregate;
  /** fetch data from the table: "rozpis_item" using primary key columns */
  rozpis_item_by_pk?: Maybe<Rozpis_Item>;
  /** fetch data from the table: "session" */
  session: Array<Session>;
  /** fetch aggregated fields from the table: "session" */
  session_aggregate: Session_Aggregate;
  /** fetch data from the table: "session" using primary key columns */
  session_by_pk?: Maybe<Session>;
  /** fetch data from the table: "skupiny" */
  skupiny: Array<Skupiny>;
  /** fetch aggregated fields from the table: "skupiny" */
  skupiny_aggregate: Skupiny_Aggregate;
  /** fetch data from the table: "skupiny" using primary key columns */
  skupiny_by_pk?: Maybe<Skupiny>;
  /** fetch data from the table: "upozorneni" */
  upozorneni: Array<Upozorneni>;
  /** fetch aggregated fields from the table: "upozorneni" */
  upozorneni_aggregate: Upozorneni_Aggregate;
  /** fetch data from the table: "upozorneni" using primary key columns */
  upozorneni_by_pk?: Maybe<Upozorneni>;
  /** fetch data from the table: "upozorneni_skupiny" */
  upozorneni_skupiny: Array<Upozorneni_Skupiny>;
  /** fetch aggregated fields from the table: "upozorneni_skupiny" */
  upozorneni_skupiny_aggregate: Upozorneni_Skupiny_Aggregate;
  /** fetch data from the table: "upozorneni_skupiny" using primary key columns */
  upozorneni_skupiny_by_pk?: Maybe<Upozorneni_Skupiny>;
  /** An array relationship */
  users: Array<Users>;
  /** An aggregate relationship */
  users_aggregate: Users_Aggregate;
  /** fetch data from the table: "users" using primary key columns */
  users_by_pk?: Maybe<Users>;
  /** fetch data from the table: "users_skupiny" */
  users_skupiny: Array<Users_Skupiny>;
  /** fetch aggregated fields from the table: "users_skupiny" */
  users_skupiny_aggregate: Users_Skupiny_Aggregate;
  /** fetch data from the table: "users_skupiny" using primary key columns */
  users_skupiny_by_pk?: Maybe<Users_Skupiny>;
  /** fetch data from the table: "video" */
  video: Array<Video>;
  /** fetch aggregated fields from the table: "video" */
  video_aggregate: Video_Aggregate;
  /** fetch data from the table: "video" using primary key columns */
  video_by_pk?: Maybe<Video>;
  /** fetch data from the table: "video_list" */
  video_list: Array<Video_List>;
  /** fetch aggregated fields from the table: "video_list" */
  video_list_aggregate: Video_List_Aggregate;
  /** fetch data from the table: "video_list" using primary key columns */
  video_list_by_pk?: Maybe<Video_List>;
  /** fetch data from the table: "video_source" */
  video_source: Array<Video_Source>;
  /** fetch aggregated fields from the table: "video_source" */
  video_source_aggregate: Video_Source_Aggregate;
  /** fetch data from the table: "video_source" using primary key columns */
  video_source_by_pk?: Maybe<Video_Source>;
};


export type Query_RootAkceArgs = {
  distinct_on?: InputMaybe<Array<Akce_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Order_By>>;
  where?: InputMaybe<Akce_Bool_Exp>;
};


export type Query_RootAkce_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Order_By>>;
  where?: InputMaybe<Akce_Bool_Exp>;
};


export type Query_RootAkce_By_PkArgs = {
  a_id: Scalars['bigint'];
};


export type Query_RootAkce_ItemArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


export type Query_RootAkce_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


export type Query_RootAkce_Item_By_PkArgs = {
  ai_id: Scalars['bigint'];
};


export type Query_RootAktualityArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


export type Query_RootAktuality_AdminArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Admin_Order_By>>;
  where?: InputMaybe<Aktuality_Admin_Bool_Exp>;
};


export type Query_RootAktuality_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Admin_Order_By>>;
  where?: InputMaybe<Aktuality_Admin_Bool_Exp>;
};


export type Query_RootAktuality_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


export type Query_RootAktuality_By_PkArgs = {
  at_id: Scalars['bigint'];
};


export type Query_RootDokumentyArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


export type Query_RootDokumenty_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


export type Query_RootDokumenty_By_PkArgs = {
  d_id: Scalars['bigint'];
};


export type Query_RootGalerie_DirArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Dir_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Dir_Order_By>>;
  where?: InputMaybe<Galerie_Dir_Bool_Exp>;
};


export type Query_RootGalerie_Dir_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Dir_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Dir_Order_By>>;
  where?: InputMaybe<Galerie_Dir_Bool_Exp>;
};


export type Query_RootGalerie_Dir_By_PkArgs = {
  gd_id: Scalars['bigint'];
};


export type Query_RootGalerie_FotoArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


export type Query_RootGalerie_Foto_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


export type Query_RootGalerie_Foto_By_PkArgs = {
  gf_id: Scalars['bigint'];
};


export type Query_RootNabidkaArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


export type Query_RootNabidka_AdminArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Admin_Order_By>>;
  where?: InputMaybe<Nabidka_Admin_Bool_Exp>;
};


export type Query_RootNabidka_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Admin_Order_By>>;
  where?: InputMaybe<Nabidka_Admin_Bool_Exp>;
};


export type Query_RootNabidka_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


export type Query_RootNabidka_By_PkArgs = {
  n_id: Scalars['bigint'];
};


export type Query_RootNabidka_ItemArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


export type Query_RootNabidka_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


export type Query_RootNabidka_Item_By_PkArgs = {
  ni_id: Scalars['bigint'];
};


export type Query_RootParametersArgs = {
  distinct_on?: InputMaybe<Array<Parameters_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Parameters_Order_By>>;
  where?: InputMaybe<Parameters_Bool_Exp>;
};


export type Query_RootParameters_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Parameters_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Parameters_Order_By>>;
  where?: InputMaybe<Parameters_Bool_Exp>;
};


export type Query_RootParameters_By_PkArgs = {
  pa_name: Scalars['String'];
};


export type Query_RootParyArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


export type Query_RootPary_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


export type Query_RootPary_By_PkArgs = {
  p_id: Scalars['bigint'];
};


export type Query_RootPary_NavrhArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


export type Query_RootPary_Navrh_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


export type Query_RootPary_Navrh_By_PkArgs = {
  pn_id: Scalars['bigint'];
};


export type Query_RootPermissionsArgs = {
  distinct_on?: InputMaybe<Array<Permissions_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Permissions_Order_By>>;
  where?: InputMaybe<Permissions_Bool_Exp>;
};


export type Query_RootPermissions_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Permissions_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Permissions_Order_By>>;
  where?: InputMaybe<Permissions_Bool_Exp>;
};


export type Query_RootPermissions_By_PkArgs = {
  pe_id: Scalars['bigint'];
};


export type Query_RootPlatby_CategoryArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Order_By>>;
  where?: InputMaybe<Platby_Category_Bool_Exp>;
};


export type Query_RootPlatby_Category_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Order_By>>;
  where?: InputMaybe<Platby_Category_Bool_Exp>;
};


export type Query_RootPlatby_Category_By_PkArgs = {
  pc_id: Scalars['bigint'];
};


export type Query_RootPlatby_Category_GroupArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


export type Query_RootPlatby_Category_Group_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


export type Query_RootPlatby_Category_Group_By_PkArgs = {
  pcg_id: Scalars['bigint'];
};


export type Query_RootPlatby_GroupArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Order_By>>;
  where?: InputMaybe<Platby_Group_Bool_Exp>;
};


export type Query_RootPlatby_Group_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Order_By>>;
  where?: InputMaybe<Platby_Group_Bool_Exp>;
};


export type Query_RootPlatby_Group_By_PkArgs = {
  pg_id: Scalars['bigint'];
};


export type Query_RootPlatby_Group_SkupinaArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


export type Query_RootPlatby_Group_Skupina_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


export type Query_RootPlatby_Group_Skupina_By_PkArgs = {
  pgs_id: Scalars['bigint'];
};


export type Query_RootPlatby_ItemArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


export type Query_RootPlatby_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


export type Query_RootPlatby_Item_By_PkArgs = {
  pi_id: Scalars['bigint'];
};


export type Query_RootPlatby_RawArgs = {
  distinct_on?: InputMaybe<Array<Platby_Raw_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Raw_Order_By>>;
  where?: InputMaybe<Platby_Raw_Bool_Exp>;
};


export type Query_RootPlatby_Raw_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Raw_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Raw_Order_By>>;
  where?: InputMaybe<Platby_Raw_Bool_Exp>;
};


export type Query_RootPlatby_Raw_By_PkArgs = {
  pr_id: Scalars['bigint'];
};


export type Query_RootRozpisArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


export type Query_RootRozpis_AdminArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Admin_Order_By>>;
  where?: InputMaybe<Rozpis_Admin_Bool_Exp>;
};


export type Query_RootRozpis_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Admin_Order_By>>;
  where?: InputMaybe<Rozpis_Admin_Bool_Exp>;
};


export type Query_RootRozpis_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


export type Query_RootRozpis_By_PkArgs = {
  r_id: Scalars['bigint'];
};


export type Query_RootRozpis_ItemArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


export type Query_RootRozpis_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


export type Query_RootRozpis_Item_By_PkArgs = {
  ri_id: Scalars['bigint'];
};


export type Query_RootSessionArgs = {
  distinct_on?: InputMaybe<Array<Session_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Session_Order_By>>;
  where?: InputMaybe<Session_Bool_Exp>;
};


export type Query_RootSession_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Session_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Session_Order_By>>;
  where?: InputMaybe<Session_Bool_Exp>;
};


export type Query_RootSession_By_PkArgs = {
  ss_id: Scalars['String'];
};


export type Query_RootSkupinyArgs = {
  distinct_on?: InputMaybe<Array<Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Skupiny_Order_By>>;
  where?: InputMaybe<Skupiny_Bool_Exp>;
};


export type Query_RootSkupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Skupiny_Order_By>>;
  where?: InputMaybe<Skupiny_Bool_Exp>;
};


export type Query_RootSkupiny_By_PkArgs = {
  s_id: Scalars['bigint'];
};


export type Query_RootUpozorneniArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};


export type Query_RootUpozorneni_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};


export type Query_RootUpozorneni_By_PkArgs = {
  up_id: Scalars['bigint'];
};


export type Query_RootUpozorneni_SkupinyArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


export type Query_RootUpozorneni_Skupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


export type Query_RootUpozorneni_Skupiny_By_PkArgs = {
  ups_id: Scalars['bigint'];
};


export type Query_RootUsersArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


export type Query_RootUsers_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


export type Query_RootUsers_By_PkArgs = {
  u_id: Scalars['bigint'];
};


export type Query_RootUsers_SkupinyArgs = {
  distinct_on?: InputMaybe<Array<Users_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Skupiny_Order_By>>;
  where?: InputMaybe<Users_Skupiny_Bool_Exp>;
};


export type Query_RootUsers_Skupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Skupiny_Order_By>>;
  where?: InputMaybe<Users_Skupiny_Bool_Exp>;
};


export type Query_RootUsers_Skupiny_By_PkArgs = {
  us_id: Scalars['bigint'];
};


export type Query_RootVideoArgs = {
  distinct_on?: InputMaybe<Array<Video_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Order_By>>;
  where?: InputMaybe<Video_Bool_Exp>;
};


export type Query_RootVideo_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Order_By>>;
  where?: InputMaybe<Video_Bool_Exp>;
};


export type Query_RootVideo_By_PkArgs = {
  v_id: Scalars['bigint'];
};


export type Query_RootVideo_ListArgs = {
  distinct_on?: InputMaybe<Array<Video_List_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_List_Order_By>>;
  where?: InputMaybe<Video_List_Bool_Exp>;
};


export type Query_RootVideo_List_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_List_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_List_Order_By>>;
  where?: InputMaybe<Video_List_Bool_Exp>;
};


export type Query_RootVideo_List_By_PkArgs = {
  vl_id: Scalars['bigint'];
};


export type Query_RootVideo_SourceArgs = {
  distinct_on?: InputMaybe<Array<Video_Source_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Source_Order_By>>;
  where?: InputMaybe<Video_Source_Bool_Exp>;
};


export type Query_RootVideo_Source_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_Source_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Source_Order_By>>;
  where?: InputMaybe<Video_Source_Bool_Exp>;
};


export type Query_RootVideo_Source_By_PkArgs = {
  vs_id: Scalars['bigint'];
};

/** columns and relationships of "rozpis" */
export type Rozpis = {
  __typename?: 'rozpis';
  r_datum: Scalars['date'];
  r_id: Scalars['bigint'];
  r_kde: Scalars['String'];
  r_lock: Scalars['Boolean'];
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener: Scalars['bigint'];
  r_visible: Scalars['Boolean'];
  /** An array relationship */
  rozpis_items: Array<Rozpis_Item>;
  /** An aggregate relationship */
  rozpis_items_aggregate: Rozpis_Item_Aggregate;
  /** An object relationship */
  user: Users;
};


/** columns and relationships of "rozpis" */
export type RozpisRozpis_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


/** columns and relationships of "rozpis" */
export type RozpisRozpis_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};

/** columns and relationships of "rozpis_admin" */
export type Rozpis_Admin = {
  __typename?: 'rozpis_admin';
  r_datum?: Maybe<Scalars['date']>;
  r_id?: Maybe<Scalars['bigint']>;
  r_kde?: Maybe<Scalars['String']>;
  r_lock?: Maybe<Scalars['Boolean']>;
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener?: Maybe<Scalars['bigint']>;
  r_visible?: Maybe<Scalars['Boolean']>;
  /** An array relationship */
  rozpis_items: Array<Rozpis_Item>;
  /** An aggregate relationship */
  rozpis_items_aggregate: Rozpis_Item_Aggregate;
  /** An object relationship */
  user?: Maybe<Users>;
};


/** columns and relationships of "rozpis_admin" */
export type Rozpis_AdminRozpis_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


/** columns and relationships of "rozpis_admin" */
export type Rozpis_AdminRozpis_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};

/** aggregated selection of "rozpis_admin" */
export type Rozpis_Admin_Aggregate = {
  __typename?: 'rozpis_admin_aggregate';
  aggregate?: Maybe<Rozpis_Admin_Aggregate_Fields>;
  nodes: Array<Rozpis_Admin>;
};

/** aggregate fields of "rozpis_admin" */
export type Rozpis_Admin_Aggregate_Fields = {
  __typename?: 'rozpis_admin_aggregate_fields';
  avg?: Maybe<Rozpis_Admin_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Rozpis_Admin_Max_Fields>;
  min?: Maybe<Rozpis_Admin_Min_Fields>;
  stddev?: Maybe<Rozpis_Admin_Stddev_Fields>;
  stddev_pop?: Maybe<Rozpis_Admin_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Rozpis_Admin_Stddev_Samp_Fields>;
  sum?: Maybe<Rozpis_Admin_Sum_Fields>;
  var_pop?: Maybe<Rozpis_Admin_Var_Pop_Fields>;
  var_samp?: Maybe<Rozpis_Admin_Var_Samp_Fields>;
  variance?: Maybe<Rozpis_Admin_Variance_Fields>;
};


/** aggregate fields of "rozpis_admin" */
export type Rozpis_Admin_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Rozpis_Admin_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Rozpis_Admin_Avg_Fields = {
  __typename?: 'rozpis_admin_avg_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "rozpis_admin". All fields are combined with a logical 'AND'. */
export type Rozpis_Admin_Bool_Exp = {
  _and?: InputMaybe<Array<Rozpis_Admin_Bool_Exp>>;
  _not?: InputMaybe<Rozpis_Admin_Bool_Exp>;
  _or?: InputMaybe<Array<Rozpis_Admin_Bool_Exp>>;
  r_datum?: InputMaybe<Date_Comparison_Exp>;
  r_id?: InputMaybe<Bigint_Comparison_Exp>;
  r_kde?: InputMaybe<String_Comparison_Exp>;
  r_lock?: InputMaybe<Boolean_Comparison_Exp>;
  r_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  r_trener?: InputMaybe<Bigint_Comparison_Exp>;
  r_visible?: InputMaybe<Boolean_Comparison_Exp>;
  rozpis_items?: InputMaybe<Rozpis_Item_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** input type for incrementing numeric columns in table "rozpis_admin" */
export type Rozpis_Admin_Inc_Input = {
  r_id?: InputMaybe<Scalars['bigint']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "rozpis_admin" */
export type Rozpis_Admin_Insert_Input = {
  r_datum?: InputMaybe<Scalars['date']>;
  r_id?: InputMaybe<Scalars['bigint']>;
  r_kde?: InputMaybe<Scalars['String']>;
  r_lock?: InputMaybe<Scalars['Boolean']>;
  r_timestamp?: InputMaybe<Scalars['timestamptz']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
  r_visible?: InputMaybe<Scalars['Boolean']>;
  rozpis_items?: InputMaybe<Rozpis_Item_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Rozpis_Admin_Max_Fields = {
  __typename?: 'rozpis_admin_max_fields';
  r_datum?: Maybe<Scalars['date']>;
  r_id?: Maybe<Scalars['bigint']>;
  r_kde?: Maybe<Scalars['String']>;
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** aggregate min on columns */
export type Rozpis_Admin_Min_Fields = {
  __typename?: 'rozpis_admin_min_fields';
  r_datum?: Maybe<Scalars['date']>;
  r_id?: Maybe<Scalars['bigint']>;
  r_kde?: Maybe<Scalars['String']>;
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** response of any mutation on the table "rozpis_admin" */
export type Rozpis_Admin_Mutation_Response = {
  __typename?: 'rozpis_admin_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Rozpis_Admin>;
};

/** Ordering options when selecting data from "rozpis_admin". */
export type Rozpis_Admin_Order_By = {
  r_datum?: InputMaybe<Order_By>;
  r_id?: InputMaybe<Order_By>;
  r_kde?: InputMaybe<Order_By>;
  r_lock?: InputMaybe<Order_By>;
  r_timestamp?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
  r_visible?: InputMaybe<Order_By>;
  rozpis_items_aggregate?: InputMaybe<Rozpis_Item_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** select columns of table "rozpis_admin" */
export enum Rozpis_Admin_Select_Column {
  /** column name */
  RDatum = 'r_datum',
  /** column name */
  RId = 'r_id',
  /** column name */
  RKde = 'r_kde',
  /** column name */
  RLock = 'r_lock',
  /** column name */
  RTimestamp = 'r_timestamp',
  /** column name */
  RTrener = 'r_trener',
  /** column name */
  RVisible = 'r_visible'
}

/** input type for updating data in table "rozpis_admin" */
export type Rozpis_Admin_Set_Input = {
  r_datum?: InputMaybe<Scalars['date']>;
  r_id?: InputMaybe<Scalars['bigint']>;
  r_kde?: InputMaybe<Scalars['String']>;
  r_lock?: InputMaybe<Scalars['Boolean']>;
  r_timestamp?: InputMaybe<Scalars['timestamptz']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
  r_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Rozpis_Admin_Stddev_Fields = {
  __typename?: 'rozpis_admin_stddev_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Rozpis_Admin_Stddev_Pop_Fields = {
  __typename?: 'rozpis_admin_stddev_pop_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Rozpis_Admin_Stddev_Samp_Fields = {
  __typename?: 'rozpis_admin_stddev_samp_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Rozpis_Admin_Sum_Fields = {
  __typename?: 'rozpis_admin_sum_fields';
  r_id?: Maybe<Scalars['bigint']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** aggregate var_pop on columns */
export type Rozpis_Admin_Var_Pop_Fields = {
  __typename?: 'rozpis_admin_var_pop_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Rozpis_Admin_Var_Samp_Fields = {
  __typename?: 'rozpis_admin_var_samp_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Rozpis_Admin_Variance_Fields = {
  __typename?: 'rozpis_admin_variance_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** aggregated selection of "rozpis" */
export type Rozpis_Aggregate = {
  __typename?: 'rozpis_aggregate';
  aggregate?: Maybe<Rozpis_Aggregate_Fields>;
  nodes: Array<Rozpis>;
};

/** aggregate fields of "rozpis" */
export type Rozpis_Aggregate_Fields = {
  __typename?: 'rozpis_aggregate_fields';
  avg?: Maybe<Rozpis_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Rozpis_Max_Fields>;
  min?: Maybe<Rozpis_Min_Fields>;
  stddev?: Maybe<Rozpis_Stddev_Fields>;
  stddev_pop?: Maybe<Rozpis_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Rozpis_Stddev_Samp_Fields>;
  sum?: Maybe<Rozpis_Sum_Fields>;
  var_pop?: Maybe<Rozpis_Var_Pop_Fields>;
  var_samp?: Maybe<Rozpis_Var_Samp_Fields>;
  variance?: Maybe<Rozpis_Variance_Fields>;
};


/** aggregate fields of "rozpis" */
export type Rozpis_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Rozpis_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "rozpis" */
export type Rozpis_Aggregate_Order_By = {
  avg?: InputMaybe<Rozpis_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Rozpis_Max_Order_By>;
  min?: InputMaybe<Rozpis_Min_Order_By>;
  stddev?: InputMaybe<Rozpis_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Rozpis_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Rozpis_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Rozpis_Sum_Order_By>;
  var_pop?: InputMaybe<Rozpis_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Rozpis_Var_Samp_Order_By>;
  variance?: InputMaybe<Rozpis_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "rozpis" */
export type Rozpis_Arr_Rel_Insert_Input = {
  data: Array<Rozpis_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Rozpis_On_Conflict>;
};

/** aggregate avg on columns */
export type Rozpis_Avg_Fields = {
  __typename?: 'rozpis_avg_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "rozpis" */
export type Rozpis_Avg_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "rozpis". All fields are combined with a logical 'AND'. */
export type Rozpis_Bool_Exp = {
  _and?: InputMaybe<Array<Rozpis_Bool_Exp>>;
  _not?: InputMaybe<Rozpis_Bool_Exp>;
  _or?: InputMaybe<Array<Rozpis_Bool_Exp>>;
  r_datum?: InputMaybe<Date_Comparison_Exp>;
  r_id?: InputMaybe<Bigint_Comparison_Exp>;
  r_kde?: InputMaybe<String_Comparison_Exp>;
  r_lock?: InputMaybe<Boolean_Comparison_Exp>;
  r_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  r_trener?: InputMaybe<Bigint_Comparison_Exp>;
  r_visible?: InputMaybe<Boolean_Comparison_Exp>;
  rozpis_items?: InputMaybe<Rozpis_Item_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "rozpis" */
export enum Rozpis_Constraint {
  /** unique or primary key constraint */
  Idx_24731Primary = 'idx_24731_primary'
}

/** input type for incrementing numeric columns in table "rozpis" */
export type Rozpis_Inc_Input = {
  r_id?: InputMaybe<Scalars['bigint']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "rozpis" */
export type Rozpis_Insert_Input = {
  r_datum?: InputMaybe<Scalars['date']>;
  r_id?: InputMaybe<Scalars['bigint']>;
  r_kde?: InputMaybe<Scalars['String']>;
  r_lock?: InputMaybe<Scalars['Boolean']>;
  r_timestamp?: InputMaybe<Scalars['timestamptz']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
  r_visible?: InputMaybe<Scalars['Boolean']>;
  rozpis_items?: InputMaybe<Rozpis_Item_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** columns and relationships of "rozpis_item" */
export type Rozpis_Item = {
  __typename?: 'rozpis_item';
  /** An object relationship */
  pary?: Maybe<Pary>;
  ri_do: Scalars['time'];
  ri_id: Scalars['bigint'];
  ri_id_rodic: Scalars['bigint'];
  ri_lock: Scalars['Boolean'];
  ri_od: Scalars['time'];
  ri_partner?: Maybe<Scalars['bigint']>;
  /** An object relationship */
  rozpi: Rozpis;
};

/** aggregated selection of "rozpis_item" */
export type Rozpis_Item_Aggregate = {
  __typename?: 'rozpis_item_aggregate';
  aggregate?: Maybe<Rozpis_Item_Aggregate_Fields>;
  nodes: Array<Rozpis_Item>;
};

/** aggregate fields of "rozpis_item" */
export type Rozpis_Item_Aggregate_Fields = {
  __typename?: 'rozpis_item_aggregate_fields';
  avg?: Maybe<Rozpis_Item_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Rozpis_Item_Max_Fields>;
  min?: Maybe<Rozpis_Item_Min_Fields>;
  stddev?: Maybe<Rozpis_Item_Stddev_Fields>;
  stddev_pop?: Maybe<Rozpis_Item_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Rozpis_Item_Stddev_Samp_Fields>;
  sum?: Maybe<Rozpis_Item_Sum_Fields>;
  var_pop?: Maybe<Rozpis_Item_Var_Pop_Fields>;
  var_samp?: Maybe<Rozpis_Item_Var_Samp_Fields>;
  variance?: Maybe<Rozpis_Item_Variance_Fields>;
};


/** aggregate fields of "rozpis_item" */
export type Rozpis_Item_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "rozpis_item" */
export type Rozpis_Item_Aggregate_Order_By = {
  avg?: InputMaybe<Rozpis_Item_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Rozpis_Item_Max_Order_By>;
  min?: InputMaybe<Rozpis_Item_Min_Order_By>;
  stddev?: InputMaybe<Rozpis_Item_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Rozpis_Item_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Rozpis_Item_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Rozpis_Item_Sum_Order_By>;
  var_pop?: InputMaybe<Rozpis_Item_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Rozpis_Item_Var_Samp_Order_By>;
  variance?: InputMaybe<Rozpis_Item_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "rozpis_item" */
export type Rozpis_Item_Arr_Rel_Insert_Input = {
  data: Array<Rozpis_Item_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Rozpis_Item_On_Conflict>;
};

/** aggregate avg on columns */
export type Rozpis_Item_Avg_Fields = {
  __typename?: 'rozpis_item_avg_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "rozpis_item" */
export type Rozpis_Item_Avg_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "rozpis_item". All fields are combined with a logical 'AND'. */
export type Rozpis_Item_Bool_Exp = {
  _and?: InputMaybe<Array<Rozpis_Item_Bool_Exp>>;
  _not?: InputMaybe<Rozpis_Item_Bool_Exp>;
  _or?: InputMaybe<Array<Rozpis_Item_Bool_Exp>>;
  pary?: InputMaybe<Pary_Bool_Exp>;
  ri_do?: InputMaybe<Time_Comparison_Exp>;
  ri_id?: InputMaybe<Bigint_Comparison_Exp>;
  ri_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  ri_lock?: InputMaybe<Boolean_Comparison_Exp>;
  ri_od?: InputMaybe<Time_Comparison_Exp>;
  ri_partner?: InputMaybe<Bigint_Comparison_Exp>;
  rozpi?: InputMaybe<Rozpis_Bool_Exp>;
};

/** unique or primary key constraints on table "rozpis_item" */
export enum Rozpis_Item_Constraint {
  /** unique or primary key constraint */
  Idx_24742Primary = 'idx_24742_primary'
}

/** input type for incrementing numeric columns in table "rozpis_item" */
export type Rozpis_Item_Inc_Input = {
  ri_id?: InputMaybe<Scalars['bigint']>;
  ri_id_rodic?: InputMaybe<Scalars['bigint']>;
  ri_partner?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "rozpis_item" */
export type Rozpis_Item_Insert_Input = {
  pary?: InputMaybe<Pary_Obj_Rel_Insert_Input>;
  ri_do?: InputMaybe<Scalars['time']>;
  ri_id?: InputMaybe<Scalars['bigint']>;
  ri_id_rodic?: InputMaybe<Scalars['bigint']>;
  ri_lock?: InputMaybe<Scalars['Boolean']>;
  ri_od?: InputMaybe<Scalars['time']>;
  ri_partner?: InputMaybe<Scalars['bigint']>;
  rozpi?: InputMaybe<Rozpis_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Rozpis_Item_Max_Fields = {
  __typename?: 'rozpis_item_max_fields';
  ri_id?: Maybe<Scalars['bigint']>;
  ri_id_rodic?: Maybe<Scalars['bigint']>;
  ri_partner?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "rozpis_item" */
export type Rozpis_Item_Max_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Rozpis_Item_Min_Fields = {
  __typename?: 'rozpis_item_min_fields';
  ri_id?: Maybe<Scalars['bigint']>;
  ri_id_rodic?: Maybe<Scalars['bigint']>;
  ri_partner?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "rozpis_item" */
export type Rozpis_Item_Min_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "rozpis_item" */
export type Rozpis_Item_Mutation_Response = {
  __typename?: 'rozpis_item_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Rozpis_Item>;
};

/** on conflict condition type for table "rozpis_item" */
export type Rozpis_Item_On_Conflict = {
  constraint: Rozpis_Item_Constraint;
  update_columns?: Array<Rozpis_Item_Update_Column>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};

/** Ordering options when selecting data from "rozpis_item". */
export type Rozpis_Item_Order_By = {
  pary?: InputMaybe<Pary_Order_By>;
  ri_do?: InputMaybe<Order_By>;
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_lock?: InputMaybe<Order_By>;
  ri_od?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
  rozpi?: InputMaybe<Rozpis_Order_By>;
};

/** primary key columns input for table: rozpis_item */
export type Rozpis_Item_Pk_Columns_Input = {
  ri_id: Scalars['bigint'];
};

/** select columns of table "rozpis_item" */
export enum Rozpis_Item_Select_Column {
  /** column name */
  RiDo = 'ri_do',
  /** column name */
  RiId = 'ri_id',
  /** column name */
  RiIdRodic = 'ri_id_rodic',
  /** column name */
  RiLock = 'ri_lock',
  /** column name */
  RiOd = 'ri_od',
  /** column name */
  RiPartner = 'ri_partner'
}

/** input type for updating data in table "rozpis_item" */
export type Rozpis_Item_Set_Input = {
  ri_do?: InputMaybe<Scalars['time']>;
  ri_id?: InputMaybe<Scalars['bigint']>;
  ri_id_rodic?: InputMaybe<Scalars['bigint']>;
  ri_lock?: InputMaybe<Scalars['Boolean']>;
  ri_od?: InputMaybe<Scalars['time']>;
  ri_partner?: InputMaybe<Scalars['bigint']>;
};

/** aggregate stddev on columns */
export type Rozpis_Item_Stddev_Fields = {
  __typename?: 'rozpis_item_stddev_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "rozpis_item" */
export type Rozpis_Item_Stddev_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Rozpis_Item_Stddev_Pop_Fields = {
  __typename?: 'rozpis_item_stddev_pop_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "rozpis_item" */
export type Rozpis_Item_Stddev_Pop_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Rozpis_Item_Stddev_Samp_Fields = {
  __typename?: 'rozpis_item_stddev_samp_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "rozpis_item" */
export type Rozpis_Item_Stddev_Samp_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Rozpis_Item_Sum_Fields = {
  __typename?: 'rozpis_item_sum_fields';
  ri_id?: Maybe<Scalars['bigint']>;
  ri_id_rodic?: Maybe<Scalars['bigint']>;
  ri_partner?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "rozpis_item" */
export type Rozpis_Item_Sum_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** update columns of table "rozpis_item" */
export enum Rozpis_Item_Update_Column {
  /** column name */
  RiDo = 'ri_do',
  /** column name */
  RiId = 'ri_id',
  /** column name */
  RiIdRodic = 'ri_id_rodic',
  /** column name */
  RiLock = 'ri_lock',
  /** column name */
  RiOd = 'ri_od',
  /** column name */
  RiPartner = 'ri_partner'
}

/** aggregate var_pop on columns */
export type Rozpis_Item_Var_Pop_Fields = {
  __typename?: 'rozpis_item_var_pop_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "rozpis_item" */
export type Rozpis_Item_Var_Pop_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Rozpis_Item_Var_Samp_Fields = {
  __typename?: 'rozpis_item_var_samp_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "rozpis_item" */
export type Rozpis_Item_Var_Samp_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Rozpis_Item_Variance_Fields = {
  __typename?: 'rozpis_item_variance_fields';
  ri_id?: Maybe<Scalars['Float']>;
  ri_id_rodic?: Maybe<Scalars['Float']>;
  ri_partner?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "rozpis_item" */
export type Rozpis_Item_Variance_Order_By = {
  ri_id?: InputMaybe<Order_By>;
  ri_id_rodic?: InputMaybe<Order_By>;
  ri_partner?: InputMaybe<Order_By>;
};

/** aggregate max on columns */
export type Rozpis_Max_Fields = {
  __typename?: 'rozpis_max_fields';
  r_datum?: Maybe<Scalars['date']>;
  r_id?: Maybe<Scalars['bigint']>;
  r_kde?: Maybe<Scalars['String']>;
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** order by max() on columns of table "rozpis" */
export type Rozpis_Max_Order_By = {
  r_datum?: InputMaybe<Order_By>;
  r_id?: InputMaybe<Order_By>;
  r_kde?: InputMaybe<Order_By>;
  r_timestamp?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Rozpis_Min_Fields = {
  __typename?: 'rozpis_min_fields';
  r_datum?: Maybe<Scalars['date']>;
  r_id?: Maybe<Scalars['bigint']>;
  r_kde?: Maybe<Scalars['String']>;
  r_timestamp?: Maybe<Scalars['timestamptz']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** order by min() on columns of table "rozpis" */
export type Rozpis_Min_Order_By = {
  r_datum?: InputMaybe<Order_By>;
  r_id?: InputMaybe<Order_By>;
  r_kde?: InputMaybe<Order_By>;
  r_timestamp?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "rozpis" */
export type Rozpis_Mutation_Response = {
  __typename?: 'rozpis_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Rozpis>;
};

/** input type for inserting object relation for remote table "rozpis" */
export type Rozpis_Obj_Rel_Insert_Input = {
  data: Rozpis_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Rozpis_On_Conflict>;
};

/** on conflict condition type for table "rozpis" */
export type Rozpis_On_Conflict = {
  constraint: Rozpis_Constraint;
  update_columns?: Array<Rozpis_Update_Column>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};

/** Ordering options when selecting data from "rozpis". */
export type Rozpis_Order_By = {
  r_datum?: InputMaybe<Order_By>;
  r_id?: InputMaybe<Order_By>;
  r_kde?: InputMaybe<Order_By>;
  r_lock?: InputMaybe<Order_By>;
  r_timestamp?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
  r_visible?: InputMaybe<Order_By>;
  rozpis_items_aggregate?: InputMaybe<Rozpis_Item_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: rozpis */
export type Rozpis_Pk_Columns_Input = {
  r_id: Scalars['bigint'];
};

/** select columns of table "rozpis" */
export enum Rozpis_Select_Column {
  /** column name */
  RDatum = 'r_datum',
  /** column name */
  RId = 'r_id',
  /** column name */
  RKde = 'r_kde',
  /** column name */
  RLock = 'r_lock',
  /** column name */
  RTimestamp = 'r_timestamp',
  /** column name */
  RTrener = 'r_trener',
  /** column name */
  RVisible = 'r_visible'
}

/** input type for updating data in table "rozpis" */
export type Rozpis_Set_Input = {
  r_datum?: InputMaybe<Scalars['date']>;
  r_id?: InputMaybe<Scalars['bigint']>;
  r_kde?: InputMaybe<Scalars['String']>;
  r_lock?: InputMaybe<Scalars['Boolean']>;
  r_timestamp?: InputMaybe<Scalars['timestamptz']>;
  r_trener?: InputMaybe<Scalars['bigint']>;
  r_visible?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate stddev on columns */
export type Rozpis_Stddev_Fields = {
  __typename?: 'rozpis_stddev_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "rozpis" */
export type Rozpis_Stddev_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Rozpis_Stddev_Pop_Fields = {
  __typename?: 'rozpis_stddev_pop_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "rozpis" */
export type Rozpis_Stddev_Pop_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Rozpis_Stddev_Samp_Fields = {
  __typename?: 'rozpis_stddev_samp_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "rozpis" */
export type Rozpis_Stddev_Samp_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Rozpis_Sum_Fields = {
  __typename?: 'rozpis_sum_fields';
  r_id?: Maybe<Scalars['bigint']>;
  r_trener?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "rozpis" */
export type Rozpis_Sum_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** update columns of table "rozpis" */
export enum Rozpis_Update_Column {
  /** column name */
  RDatum = 'r_datum',
  /** column name */
  RId = 'r_id',
  /** column name */
  RKde = 'r_kde',
  /** column name */
  RLock = 'r_lock',
  /** column name */
  RTimestamp = 'r_timestamp',
  /** column name */
  RTrener = 'r_trener',
  /** column name */
  RVisible = 'r_visible'
}

/** aggregate var_pop on columns */
export type Rozpis_Var_Pop_Fields = {
  __typename?: 'rozpis_var_pop_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "rozpis" */
export type Rozpis_Var_Pop_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Rozpis_Var_Samp_Fields = {
  __typename?: 'rozpis_var_samp_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "rozpis" */
export type Rozpis_Var_Samp_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Rozpis_Variance_Fields = {
  __typename?: 'rozpis_variance_fields';
  r_id?: Maybe<Scalars['Float']>;
  r_trener?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "rozpis" */
export type Rozpis_Variance_Order_By = {
  r_id?: InputMaybe<Order_By>;
  r_trener?: InputMaybe<Order_By>;
};

/** columns and relationships of "session" */
export type Session = {
  __typename?: 'session';
  ss_data: Scalars['bytea'];
  ss_id: Scalars['String'];
  ss_lifetime: Scalars['bigint'];
  ss_updated_at: Scalars['timestamptz'];
};

/** aggregated selection of "session" */
export type Session_Aggregate = {
  __typename?: 'session_aggregate';
  aggregate?: Maybe<Session_Aggregate_Fields>;
  nodes: Array<Session>;
};

/** aggregate fields of "session" */
export type Session_Aggregate_Fields = {
  __typename?: 'session_aggregate_fields';
  avg?: Maybe<Session_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Session_Max_Fields>;
  min?: Maybe<Session_Min_Fields>;
  stddev?: Maybe<Session_Stddev_Fields>;
  stddev_pop?: Maybe<Session_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Session_Stddev_Samp_Fields>;
  sum?: Maybe<Session_Sum_Fields>;
  var_pop?: Maybe<Session_Var_Pop_Fields>;
  var_samp?: Maybe<Session_Var_Samp_Fields>;
  variance?: Maybe<Session_Variance_Fields>;
};


/** aggregate fields of "session" */
export type Session_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Session_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Session_Avg_Fields = {
  __typename?: 'session_avg_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "session". All fields are combined with a logical 'AND'. */
export type Session_Bool_Exp = {
  _and?: InputMaybe<Array<Session_Bool_Exp>>;
  _not?: InputMaybe<Session_Bool_Exp>;
  _or?: InputMaybe<Array<Session_Bool_Exp>>;
  ss_data?: InputMaybe<Bytea_Comparison_Exp>;
  ss_id?: InputMaybe<String_Comparison_Exp>;
  ss_lifetime?: InputMaybe<Bigint_Comparison_Exp>;
  ss_updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
};

/** unique or primary key constraints on table "session" */
export enum Session_Constraint {
  /** unique or primary key constraint */
  Idx_24747Primary = 'idx_24747_primary'
}

/** input type for incrementing numeric columns in table "session" */
export type Session_Inc_Input = {
  ss_lifetime?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "session" */
export type Session_Insert_Input = {
  ss_data?: InputMaybe<Scalars['bytea']>;
  ss_id?: InputMaybe<Scalars['String']>;
  ss_lifetime?: InputMaybe<Scalars['bigint']>;
  ss_updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate max on columns */
export type Session_Max_Fields = {
  __typename?: 'session_max_fields';
  ss_id?: Maybe<Scalars['String']>;
  ss_lifetime?: Maybe<Scalars['bigint']>;
  ss_updated_at?: Maybe<Scalars['timestamptz']>;
};

/** aggregate min on columns */
export type Session_Min_Fields = {
  __typename?: 'session_min_fields';
  ss_id?: Maybe<Scalars['String']>;
  ss_lifetime?: Maybe<Scalars['bigint']>;
  ss_updated_at?: Maybe<Scalars['timestamptz']>;
};

/** response of any mutation on the table "session" */
export type Session_Mutation_Response = {
  __typename?: 'session_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Session>;
};

/** on conflict condition type for table "session" */
export type Session_On_Conflict = {
  constraint: Session_Constraint;
  update_columns?: Array<Session_Update_Column>;
  where?: InputMaybe<Session_Bool_Exp>;
};

/** Ordering options when selecting data from "session". */
export type Session_Order_By = {
  ss_data?: InputMaybe<Order_By>;
  ss_id?: InputMaybe<Order_By>;
  ss_lifetime?: InputMaybe<Order_By>;
  ss_updated_at?: InputMaybe<Order_By>;
};

/** primary key columns input for table: session */
export type Session_Pk_Columns_Input = {
  ss_id: Scalars['String'];
};

/** select columns of table "session" */
export enum Session_Select_Column {
  /** column name */
  SsData = 'ss_data',
  /** column name */
  SsId = 'ss_id',
  /** column name */
  SsLifetime = 'ss_lifetime',
  /** column name */
  SsUpdatedAt = 'ss_updated_at'
}

/** input type for updating data in table "session" */
export type Session_Set_Input = {
  ss_data?: InputMaybe<Scalars['bytea']>;
  ss_id?: InputMaybe<Scalars['String']>;
  ss_lifetime?: InputMaybe<Scalars['bigint']>;
  ss_updated_at?: InputMaybe<Scalars['timestamptz']>;
};

/** aggregate stddev on columns */
export type Session_Stddev_Fields = {
  __typename?: 'session_stddev_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Session_Stddev_Pop_Fields = {
  __typename?: 'session_stddev_pop_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Session_Stddev_Samp_Fields = {
  __typename?: 'session_stddev_samp_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Session_Sum_Fields = {
  __typename?: 'session_sum_fields';
  ss_lifetime?: Maybe<Scalars['bigint']>;
};

/** update columns of table "session" */
export enum Session_Update_Column {
  /** column name */
  SsData = 'ss_data',
  /** column name */
  SsId = 'ss_id',
  /** column name */
  SsLifetime = 'ss_lifetime',
  /** column name */
  SsUpdatedAt = 'ss_updated_at'
}

/** aggregate var_pop on columns */
export type Session_Var_Pop_Fields = {
  __typename?: 'session_var_pop_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Session_Var_Samp_Fields = {
  __typename?: 'session_var_samp_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Session_Variance_Fields = {
  __typename?: 'session_variance_fields';
  ss_lifetime?: Maybe<Scalars['Float']>;
};

/** columns and relationships of "skupiny" */
export type Skupiny = {
  __typename?: 'skupiny';
  /** An array relationship */
  platby_group_skupinas: Array<Platby_Group_Skupina>;
  /** An aggregate relationship */
  platby_group_skupinas_aggregate: Platby_Group_Skupina_Aggregate;
  s_color_rgb: Scalars['String'];
  s_color_text: Scalars['String'];
  s_description: Scalars['String'];
  s_id: Scalars['bigint'];
  s_name: Scalars['String'];
  /** An array relationship */
  upozorneni_skupinies: Array<Upozorneni_Skupiny>;
  /** An aggregate relationship */
  upozorneni_skupinies_aggregate: Upozorneni_Skupiny_Aggregate;
  /** An array relationship */
  users: Array<Users>;
  /** An aggregate relationship */
  users_aggregate: Users_Aggregate;
};


/** columns and relationships of "skupiny" */
export type SkupinyPlatby_Group_SkupinasArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


/** columns and relationships of "skupiny" */
export type SkupinyPlatby_Group_Skupinas_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


/** columns and relationships of "skupiny" */
export type SkupinyUpozorneni_SkupiniesArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


/** columns and relationships of "skupiny" */
export type SkupinyUpozorneni_Skupinies_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


/** columns and relationships of "skupiny" */
export type SkupinyUsersArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


/** columns and relationships of "skupiny" */
export type SkupinyUsers_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};

/** aggregated selection of "skupiny" */
export type Skupiny_Aggregate = {
  __typename?: 'skupiny_aggregate';
  aggregate?: Maybe<Skupiny_Aggregate_Fields>;
  nodes: Array<Skupiny>;
};

/** aggregate fields of "skupiny" */
export type Skupiny_Aggregate_Fields = {
  __typename?: 'skupiny_aggregate_fields';
  avg?: Maybe<Skupiny_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Skupiny_Max_Fields>;
  min?: Maybe<Skupiny_Min_Fields>;
  stddev?: Maybe<Skupiny_Stddev_Fields>;
  stddev_pop?: Maybe<Skupiny_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Skupiny_Stddev_Samp_Fields>;
  sum?: Maybe<Skupiny_Sum_Fields>;
  var_pop?: Maybe<Skupiny_Var_Pop_Fields>;
  var_samp?: Maybe<Skupiny_Var_Samp_Fields>;
  variance?: Maybe<Skupiny_Variance_Fields>;
};


/** aggregate fields of "skupiny" */
export type Skupiny_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Skupiny_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Skupiny_Avg_Fields = {
  __typename?: 'skupiny_avg_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "skupiny". All fields are combined with a logical 'AND'. */
export type Skupiny_Bool_Exp = {
  _and?: InputMaybe<Array<Skupiny_Bool_Exp>>;
  _not?: InputMaybe<Skupiny_Bool_Exp>;
  _or?: InputMaybe<Array<Skupiny_Bool_Exp>>;
  platby_group_skupinas?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
  s_color_rgb?: InputMaybe<String_Comparison_Exp>;
  s_color_text?: InputMaybe<String_Comparison_Exp>;
  s_description?: InputMaybe<String_Comparison_Exp>;
  s_id?: InputMaybe<Bigint_Comparison_Exp>;
  s_name?: InputMaybe<String_Comparison_Exp>;
  upozorneni_skupinies?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
  users?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "skupiny" */
export enum Skupiny_Constraint {
  /** unique or primary key constraint */
  Idx_24756Primary = 'idx_24756_primary'
}

/** input type for incrementing numeric columns in table "skupiny" */
export type Skupiny_Inc_Input = {
  s_id?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "skupiny" */
export type Skupiny_Insert_Input = {
  platby_group_skupinas?: InputMaybe<Platby_Group_Skupina_Arr_Rel_Insert_Input>;
  s_color_rgb?: InputMaybe<Scalars['String']>;
  s_color_text?: InputMaybe<Scalars['String']>;
  s_description?: InputMaybe<Scalars['String']>;
  s_id?: InputMaybe<Scalars['bigint']>;
  s_name?: InputMaybe<Scalars['String']>;
  upozorneni_skupinies?: InputMaybe<Upozorneni_Skupiny_Arr_Rel_Insert_Input>;
  users?: InputMaybe<Users_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Skupiny_Max_Fields = {
  __typename?: 'skupiny_max_fields';
  s_color_rgb?: Maybe<Scalars['String']>;
  s_color_text?: Maybe<Scalars['String']>;
  s_description?: Maybe<Scalars['String']>;
  s_id?: Maybe<Scalars['bigint']>;
  s_name?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Skupiny_Min_Fields = {
  __typename?: 'skupiny_min_fields';
  s_color_rgb?: Maybe<Scalars['String']>;
  s_color_text?: Maybe<Scalars['String']>;
  s_description?: Maybe<Scalars['String']>;
  s_id?: Maybe<Scalars['bigint']>;
  s_name?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "skupiny" */
export type Skupiny_Mutation_Response = {
  __typename?: 'skupiny_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Skupiny>;
};

/** input type for inserting object relation for remote table "skupiny" */
export type Skupiny_Obj_Rel_Insert_Input = {
  data: Skupiny_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Skupiny_On_Conflict>;
};

/** on conflict condition type for table "skupiny" */
export type Skupiny_On_Conflict = {
  constraint: Skupiny_Constraint;
  update_columns?: Array<Skupiny_Update_Column>;
  where?: InputMaybe<Skupiny_Bool_Exp>;
};

/** Ordering options when selecting data from "skupiny". */
export type Skupiny_Order_By = {
  platby_group_skupinas_aggregate?: InputMaybe<Platby_Group_Skupina_Aggregate_Order_By>;
  s_color_rgb?: InputMaybe<Order_By>;
  s_color_text?: InputMaybe<Order_By>;
  s_description?: InputMaybe<Order_By>;
  s_id?: InputMaybe<Order_By>;
  s_name?: InputMaybe<Order_By>;
  upozorneni_skupinies_aggregate?: InputMaybe<Upozorneni_Skupiny_Aggregate_Order_By>;
  users_aggregate?: InputMaybe<Users_Aggregate_Order_By>;
};

/** primary key columns input for table: skupiny */
export type Skupiny_Pk_Columns_Input = {
  s_id: Scalars['bigint'];
};

/** select columns of table "skupiny" */
export enum Skupiny_Select_Column {
  /** column name */
  SColorRgb = 's_color_rgb',
  /** column name */
  SColorText = 's_color_text',
  /** column name */
  SDescription = 's_description',
  /** column name */
  SId = 's_id',
  /** column name */
  SName = 's_name'
}

/** input type for updating data in table "skupiny" */
export type Skupiny_Set_Input = {
  s_color_rgb?: InputMaybe<Scalars['String']>;
  s_color_text?: InputMaybe<Scalars['String']>;
  s_description?: InputMaybe<Scalars['String']>;
  s_id?: InputMaybe<Scalars['bigint']>;
  s_name?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Skupiny_Stddev_Fields = {
  __typename?: 'skupiny_stddev_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Skupiny_Stddev_Pop_Fields = {
  __typename?: 'skupiny_stddev_pop_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Skupiny_Stddev_Samp_Fields = {
  __typename?: 'skupiny_stddev_samp_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Skupiny_Sum_Fields = {
  __typename?: 'skupiny_sum_fields';
  s_id?: Maybe<Scalars['bigint']>;
};

/** update columns of table "skupiny" */
export enum Skupiny_Update_Column {
  /** column name */
  SColorRgb = 's_color_rgb',
  /** column name */
  SColorText = 's_color_text',
  /** column name */
  SDescription = 's_description',
  /** column name */
  SId = 's_id',
  /** column name */
  SName = 's_name'
}

/** aggregate var_pop on columns */
export type Skupiny_Var_Pop_Fields = {
  __typename?: 'skupiny_var_pop_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Skupiny_Var_Samp_Fields = {
  __typename?: 'skupiny_var_samp_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Skupiny_Variance_Fields = {
  __typename?: 'skupiny_variance_fields';
  s_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to compare columns of type "smallint". All fields are combined with logical 'AND'. */
export type Smallint_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['smallint']>;
  _gt?: InputMaybe<Scalars['smallint']>;
  _gte?: InputMaybe<Scalars['smallint']>;
  _in?: InputMaybe<Array<Scalars['smallint']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['smallint']>;
  _lte?: InputMaybe<Scalars['smallint']>;
  _neq?: InputMaybe<Scalars['smallint']>;
  _nin?: InputMaybe<Array<Scalars['smallint']>>;
};

export type Subscription_Root = {
  __typename?: 'subscription_root';
  /** fetch data from the table: "akce" */
  akce: Array<Akce>;
  /** fetch aggregated fields from the table: "akce" */
  akce_aggregate: Akce_Aggregate;
  /** fetch data from the table: "akce" using primary key columns */
  akce_by_pk?: Maybe<Akce>;
  /** fetch data from the table: "akce_item" */
  akce_item: Array<Akce_Item>;
  /** fetch aggregated fields from the table: "akce_item" */
  akce_item_aggregate: Akce_Item_Aggregate;
  /** fetch data from the table: "akce_item" using primary key columns */
  akce_item_by_pk?: Maybe<Akce_Item>;
  /** fetch data from the table: "aktuality" */
  aktuality: Array<Aktuality>;
  /** fetch data from the table: "aktuality_admin" */
  aktuality_admin: Array<Aktuality_Admin>;
  /** fetch aggregated fields from the table: "aktuality_admin" */
  aktuality_admin_aggregate: Aktuality_Admin_Aggregate;
  /** fetch aggregated fields from the table: "aktuality" */
  aktuality_aggregate: Aktuality_Aggregate;
  /** fetch data from the table: "aktuality" using primary key columns */
  aktuality_by_pk?: Maybe<Aktuality>;
  /** fetch data from the table: "dokumenty" */
  dokumenty: Array<Dokumenty>;
  /** fetch aggregated fields from the table: "dokumenty" */
  dokumenty_aggregate: Dokumenty_Aggregate;
  /** fetch data from the table: "dokumenty" using primary key columns */
  dokumenty_by_pk?: Maybe<Dokumenty>;
  /** fetch data from the table: "galerie_dir" */
  galerie_dir: Array<Galerie_Dir>;
  /** fetch aggregated fields from the table: "galerie_dir" */
  galerie_dir_aggregate: Galerie_Dir_Aggregate;
  /** fetch data from the table: "galerie_dir" using primary key columns */
  galerie_dir_by_pk?: Maybe<Galerie_Dir>;
  /** fetch data from the table: "galerie_foto" */
  galerie_foto: Array<Galerie_Foto>;
  /** fetch aggregated fields from the table: "galerie_foto" */
  galerie_foto_aggregate: Galerie_Foto_Aggregate;
  /** fetch data from the table: "galerie_foto" using primary key columns */
  galerie_foto_by_pk?: Maybe<Galerie_Foto>;
  /** fetch data from the table: "nabidka" */
  nabidka: Array<Nabidka>;
  /** fetch data from the table: "nabidka_admin" */
  nabidka_admin: Array<Nabidka_Admin>;
  /** fetch aggregated fields from the table: "nabidka_admin" */
  nabidka_admin_aggregate: Nabidka_Admin_Aggregate;
  /** fetch aggregated fields from the table: "nabidka" */
  nabidka_aggregate: Nabidka_Aggregate;
  /** fetch data from the table: "nabidka" using primary key columns */
  nabidka_by_pk?: Maybe<Nabidka>;
  /** fetch data from the table: "nabidka_item" */
  nabidka_item: Array<Nabidka_Item>;
  /** fetch aggregated fields from the table: "nabidka_item" */
  nabidka_item_aggregate: Nabidka_Item_Aggregate;
  /** fetch data from the table: "nabidka_item" using primary key columns */
  nabidka_item_by_pk?: Maybe<Nabidka_Item>;
  /** fetch data from the table: "parameters" */
  parameters: Array<Parameters>;
  /** fetch aggregated fields from the table: "parameters" */
  parameters_aggregate: Parameters_Aggregate;
  /** fetch data from the table: "parameters" using primary key columns */
  parameters_by_pk?: Maybe<Parameters>;
  /** fetch data from the table: "pary" */
  pary: Array<Pary>;
  /** fetch aggregated fields from the table: "pary" */
  pary_aggregate: Pary_Aggregate;
  /** fetch data from the table: "pary" using primary key columns */
  pary_by_pk?: Maybe<Pary>;
  /** fetch data from the table: "pary_navrh" */
  pary_navrh: Array<Pary_Navrh>;
  /** fetch aggregated fields from the table: "pary_navrh" */
  pary_navrh_aggregate: Pary_Navrh_Aggregate;
  /** fetch data from the table: "pary_navrh" using primary key columns */
  pary_navrh_by_pk?: Maybe<Pary_Navrh>;
  /** fetch data from the table: "permissions" */
  permissions: Array<Permissions>;
  /** fetch aggregated fields from the table: "permissions" */
  permissions_aggregate: Permissions_Aggregate;
  /** fetch data from the table: "permissions" using primary key columns */
  permissions_by_pk?: Maybe<Permissions>;
  /** fetch data from the table: "platby_category" */
  platby_category: Array<Platby_Category>;
  /** fetch aggregated fields from the table: "platby_category" */
  platby_category_aggregate: Platby_Category_Aggregate;
  /** fetch data from the table: "platby_category" using primary key columns */
  platby_category_by_pk?: Maybe<Platby_Category>;
  /** fetch data from the table: "platby_category_group" */
  platby_category_group: Array<Platby_Category_Group>;
  /** fetch aggregated fields from the table: "platby_category_group" */
  platby_category_group_aggregate: Platby_Category_Group_Aggregate;
  /** fetch data from the table: "platby_category_group" using primary key columns */
  platby_category_group_by_pk?: Maybe<Platby_Category_Group>;
  /** fetch data from the table: "platby_group" */
  platby_group: Array<Platby_Group>;
  /** fetch aggregated fields from the table: "platby_group" */
  platby_group_aggregate: Platby_Group_Aggregate;
  /** fetch data from the table: "platby_group" using primary key columns */
  platby_group_by_pk?: Maybe<Platby_Group>;
  /** fetch data from the table: "platby_group_skupina" */
  platby_group_skupina: Array<Platby_Group_Skupina>;
  /** fetch aggregated fields from the table: "platby_group_skupina" */
  platby_group_skupina_aggregate: Platby_Group_Skupina_Aggregate;
  /** fetch data from the table: "platby_group_skupina" using primary key columns */
  platby_group_skupina_by_pk?: Maybe<Platby_Group_Skupina>;
  /** fetch data from the table: "platby_item" */
  platby_item: Array<Platby_Item>;
  /** fetch aggregated fields from the table: "platby_item" */
  platby_item_aggregate: Platby_Item_Aggregate;
  /** fetch data from the table: "platby_item" using primary key columns */
  platby_item_by_pk?: Maybe<Platby_Item>;
  /** fetch data from the table: "platby_raw" */
  platby_raw: Array<Platby_Raw>;
  /** fetch aggregated fields from the table: "platby_raw" */
  platby_raw_aggregate: Platby_Raw_Aggregate;
  /** fetch data from the table: "platby_raw" using primary key columns */
  platby_raw_by_pk?: Maybe<Platby_Raw>;
  /** An array relationship */
  rozpis: Array<Rozpis>;
  /** fetch data from the table: "rozpis_admin" */
  rozpis_admin: Array<Rozpis_Admin>;
  /** fetch aggregated fields from the table: "rozpis_admin" */
  rozpis_admin_aggregate: Rozpis_Admin_Aggregate;
  /** An aggregate relationship */
  rozpis_aggregate: Rozpis_Aggregate;
  /** fetch data from the table: "rozpis" using primary key columns */
  rozpis_by_pk?: Maybe<Rozpis>;
  /** fetch data from the table: "rozpis_item" */
  rozpis_item: Array<Rozpis_Item>;
  /** fetch aggregated fields from the table: "rozpis_item" */
  rozpis_item_aggregate: Rozpis_Item_Aggregate;
  /** fetch data from the table: "rozpis_item" using primary key columns */
  rozpis_item_by_pk?: Maybe<Rozpis_Item>;
  /** fetch data from the table: "session" */
  session: Array<Session>;
  /** fetch aggregated fields from the table: "session" */
  session_aggregate: Session_Aggregate;
  /** fetch data from the table: "session" using primary key columns */
  session_by_pk?: Maybe<Session>;
  /** fetch data from the table: "skupiny" */
  skupiny: Array<Skupiny>;
  /** fetch aggregated fields from the table: "skupiny" */
  skupiny_aggregate: Skupiny_Aggregate;
  /** fetch data from the table: "skupiny" using primary key columns */
  skupiny_by_pk?: Maybe<Skupiny>;
  /** fetch data from the table: "upozorneni" */
  upozorneni: Array<Upozorneni>;
  /** fetch aggregated fields from the table: "upozorneni" */
  upozorneni_aggregate: Upozorneni_Aggregate;
  /** fetch data from the table: "upozorneni" using primary key columns */
  upozorneni_by_pk?: Maybe<Upozorneni>;
  /** fetch data from the table: "upozorneni_skupiny" */
  upozorneni_skupiny: Array<Upozorneni_Skupiny>;
  /** fetch aggregated fields from the table: "upozorneni_skupiny" */
  upozorneni_skupiny_aggregate: Upozorneni_Skupiny_Aggregate;
  /** fetch data from the table: "upozorneni_skupiny" using primary key columns */
  upozorneni_skupiny_by_pk?: Maybe<Upozorneni_Skupiny>;
  /** An array relationship */
  users: Array<Users>;
  /** An aggregate relationship */
  users_aggregate: Users_Aggregate;
  /** fetch data from the table: "users" using primary key columns */
  users_by_pk?: Maybe<Users>;
  /** fetch data from the table: "users_skupiny" */
  users_skupiny: Array<Users_Skupiny>;
  /** fetch aggregated fields from the table: "users_skupiny" */
  users_skupiny_aggregate: Users_Skupiny_Aggregate;
  /** fetch data from the table: "users_skupiny" using primary key columns */
  users_skupiny_by_pk?: Maybe<Users_Skupiny>;
  /** fetch data from the table: "video" */
  video: Array<Video>;
  /** fetch aggregated fields from the table: "video" */
  video_aggregate: Video_Aggregate;
  /** fetch data from the table: "video" using primary key columns */
  video_by_pk?: Maybe<Video>;
  /** fetch data from the table: "video_list" */
  video_list: Array<Video_List>;
  /** fetch aggregated fields from the table: "video_list" */
  video_list_aggregate: Video_List_Aggregate;
  /** fetch data from the table: "video_list" using primary key columns */
  video_list_by_pk?: Maybe<Video_List>;
  /** fetch data from the table: "video_source" */
  video_source: Array<Video_Source>;
  /** fetch aggregated fields from the table: "video_source" */
  video_source_aggregate: Video_Source_Aggregate;
  /** fetch data from the table: "video_source" using primary key columns */
  video_source_by_pk?: Maybe<Video_Source>;
};


export type Subscription_RootAkceArgs = {
  distinct_on?: InputMaybe<Array<Akce_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Order_By>>;
  where?: InputMaybe<Akce_Bool_Exp>;
};


export type Subscription_RootAkce_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Order_By>>;
  where?: InputMaybe<Akce_Bool_Exp>;
};


export type Subscription_RootAkce_By_PkArgs = {
  a_id: Scalars['bigint'];
};


export type Subscription_RootAkce_ItemArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


export type Subscription_RootAkce_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


export type Subscription_RootAkce_Item_By_PkArgs = {
  ai_id: Scalars['bigint'];
};


export type Subscription_RootAktualityArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


export type Subscription_RootAktuality_AdminArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Admin_Order_By>>;
  where?: InputMaybe<Aktuality_Admin_Bool_Exp>;
};


export type Subscription_RootAktuality_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Admin_Order_By>>;
  where?: InputMaybe<Aktuality_Admin_Bool_Exp>;
};


export type Subscription_RootAktuality_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


export type Subscription_RootAktuality_By_PkArgs = {
  at_id: Scalars['bigint'];
};


export type Subscription_RootDokumentyArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


export type Subscription_RootDokumenty_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


export type Subscription_RootDokumenty_By_PkArgs = {
  d_id: Scalars['bigint'];
};


export type Subscription_RootGalerie_DirArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Dir_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Dir_Order_By>>;
  where?: InputMaybe<Galerie_Dir_Bool_Exp>;
};


export type Subscription_RootGalerie_Dir_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Dir_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Dir_Order_By>>;
  where?: InputMaybe<Galerie_Dir_Bool_Exp>;
};


export type Subscription_RootGalerie_Dir_By_PkArgs = {
  gd_id: Scalars['bigint'];
};


export type Subscription_RootGalerie_FotoArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


export type Subscription_RootGalerie_Foto_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


export type Subscription_RootGalerie_Foto_By_PkArgs = {
  gf_id: Scalars['bigint'];
};


export type Subscription_RootNabidkaArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


export type Subscription_RootNabidka_AdminArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Admin_Order_By>>;
  where?: InputMaybe<Nabidka_Admin_Bool_Exp>;
};


export type Subscription_RootNabidka_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Admin_Order_By>>;
  where?: InputMaybe<Nabidka_Admin_Bool_Exp>;
};


export type Subscription_RootNabidka_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


export type Subscription_RootNabidka_By_PkArgs = {
  n_id: Scalars['bigint'];
};


export type Subscription_RootNabidka_ItemArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


export type Subscription_RootNabidka_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Item_Order_By>>;
  where?: InputMaybe<Nabidka_Item_Bool_Exp>;
};


export type Subscription_RootNabidka_Item_By_PkArgs = {
  ni_id: Scalars['bigint'];
};


export type Subscription_RootParametersArgs = {
  distinct_on?: InputMaybe<Array<Parameters_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Parameters_Order_By>>;
  where?: InputMaybe<Parameters_Bool_Exp>;
};


export type Subscription_RootParameters_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Parameters_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Parameters_Order_By>>;
  where?: InputMaybe<Parameters_Bool_Exp>;
};


export type Subscription_RootParameters_By_PkArgs = {
  pa_name: Scalars['String'];
};


export type Subscription_RootParyArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


export type Subscription_RootPary_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


export type Subscription_RootPary_By_PkArgs = {
  p_id: Scalars['bigint'];
};


export type Subscription_RootPary_NavrhArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


export type Subscription_RootPary_Navrh_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


export type Subscription_RootPary_Navrh_By_PkArgs = {
  pn_id: Scalars['bigint'];
};


export type Subscription_RootPermissionsArgs = {
  distinct_on?: InputMaybe<Array<Permissions_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Permissions_Order_By>>;
  where?: InputMaybe<Permissions_Bool_Exp>;
};


export type Subscription_RootPermissions_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Permissions_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Permissions_Order_By>>;
  where?: InputMaybe<Permissions_Bool_Exp>;
};


export type Subscription_RootPermissions_By_PkArgs = {
  pe_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_CategoryArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Order_By>>;
  where?: InputMaybe<Platby_Category_Bool_Exp>;
};


export type Subscription_RootPlatby_Category_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Order_By>>;
  where?: InputMaybe<Platby_Category_Bool_Exp>;
};


export type Subscription_RootPlatby_Category_By_PkArgs = {
  pc_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_Category_GroupArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


export type Subscription_RootPlatby_Category_Group_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Category_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Category_Group_Order_By>>;
  where?: InputMaybe<Platby_Category_Group_Bool_Exp>;
};


export type Subscription_RootPlatby_Category_Group_By_PkArgs = {
  pcg_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_GroupArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Order_By>>;
  where?: InputMaybe<Platby_Group_Bool_Exp>;
};


export type Subscription_RootPlatby_Group_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Order_By>>;
  where?: InputMaybe<Platby_Group_Bool_Exp>;
};


export type Subscription_RootPlatby_Group_By_PkArgs = {
  pg_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_Group_SkupinaArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


export type Subscription_RootPlatby_Group_Skupina_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Group_Skupina_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Group_Skupina_Order_By>>;
  where?: InputMaybe<Platby_Group_Skupina_Bool_Exp>;
};


export type Subscription_RootPlatby_Group_Skupina_By_PkArgs = {
  pgs_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_ItemArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


export type Subscription_RootPlatby_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


export type Subscription_RootPlatby_Item_By_PkArgs = {
  pi_id: Scalars['bigint'];
};


export type Subscription_RootPlatby_RawArgs = {
  distinct_on?: InputMaybe<Array<Platby_Raw_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Raw_Order_By>>;
  where?: InputMaybe<Platby_Raw_Bool_Exp>;
};


export type Subscription_RootPlatby_Raw_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Raw_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Raw_Order_By>>;
  where?: InputMaybe<Platby_Raw_Bool_Exp>;
};


export type Subscription_RootPlatby_Raw_By_PkArgs = {
  pr_id: Scalars['bigint'];
};


export type Subscription_RootRozpisArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


export type Subscription_RootRozpis_AdminArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Admin_Order_By>>;
  where?: InputMaybe<Rozpis_Admin_Bool_Exp>;
};


export type Subscription_RootRozpis_Admin_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Admin_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Admin_Order_By>>;
  where?: InputMaybe<Rozpis_Admin_Bool_Exp>;
};


export type Subscription_RootRozpis_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


export type Subscription_RootRozpis_By_PkArgs = {
  r_id: Scalars['bigint'];
};


export type Subscription_RootRozpis_ItemArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


export type Subscription_RootRozpis_Item_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Item_Order_By>>;
  where?: InputMaybe<Rozpis_Item_Bool_Exp>;
};


export type Subscription_RootRozpis_Item_By_PkArgs = {
  ri_id: Scalars['bigint'];
};


export type Subscription_RootSessionArgs = {
  distinct_on?: InputMaybe<Array<Session_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Session_Order_By>>;
  where?: InputMaybe<Session_Bool_Exp>;
};


export type Subscription_RootSession_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Session_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Session_Order_By>>;
  where?: InputMaybe<Session_Bool_Exp>;
};


export type Subscription_RootSession_By_PkArgs = {
  ss_id: Scalars['String'];
};


export type Subscription_RootSkupinyArgs = {
  distinct_on?: InputMaybe<Array<Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Skupiny_Order_By>>;
  where?: InputMaybe<Skupiny_Bool_Exp>;
};


export type Subscription_RootSkupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Skupiny_Order_By>>;
  where?: InputMaybe<Skupiny_Bool_Exp>;
};


export type Subscription_RootSkupiny_By_PkArgs = {
  s_id: Scalars['bigint'];
};


export type Subscription_RootUpozorneniArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};


export type Subscription_RootUpozorneni_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};


export type Subscription_RootUpozorneni_By_PkArgs = {
  up_id: Scalars['bigint'];
};


export type Subscription_RootUpozorneni_SkupinyArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


export type Subscription_RootUpozorneni_Skupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


export type Subscription_RootUpozorneni_Skupiny_By_PkArgs = {
  ups_id: Scalars['bigint'];
};


export type Subscription_RootUsersArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


export type Subscription_RootUsers_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Order_By>>;
  where?: InputMaybe<Users_Bool_Exp>;
};


export type Subscription_RootUsers_By_PkArgs = {
  u_id: Scalars['bigint'];
};


export type Subscription_RootUsers_SkupinyArgs = {
  distinct_on?: InputMaybe<Array<Users_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Skupiny_Order_By>>;
  where?: InputMaybe<Users_Skupiny_Bool_Exp>;
};


export type Subscription_RootUsers_Skupiny_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Users_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Users_Skupiny_Order_By>>;
  where?: InputMaybe<Users_Skupiny_Bool_Exp>;
};


export type Subscription_RootUsers_Skupiny_By_PkArgs = {
  us_id: Scalars['bigint'];
};


export type Subscription_RootVideoArgs = {
  distinct_on?: InputMaybe<Array<Video_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Order_By>>;
  where?: InputMaybe<Video_Bool_Exp>;
};


export type Subscription_RootVideo_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Order_By>>;
  where?: InputMaybe<Video_Bool_Exp>;
};


export type Subscription_RootVideo_By_PkArgs = {
  v_id: Scalars['bigint'];
};


export type Subscription_RootVideo_ListArgs = {
  distinct_on?: InputMaybe<Array<Video_List_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_List_Order_By>>;
  where?: InputMaybe<Video_List_Bool_Exp>;
};


export type Subscription_RootVideo_List_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_List_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_List_Order_By>>;
  where?: InputMaybe<Video_List_Bool_Exp>;
};


export type Subscription_RootVideo_List_By_PkArgs = {
  vl_id: Scalars['bigint'];
};


export type Subscription_RootVideo_SourceArgs = {
  distinct_on?: InputMaybe<Array<Video_Source_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Source_Order_By>>;
  where?: InputMaybe<Video_Source_Bool_Exp>;
};


export type Subscription_RootVideo_Source_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Video_Source_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Video_Source_Order_By>>;
  where?: InputMaybe<Video_Source_Bool_Exp>;
};


export type Subscription_RootVideo_Source_By_PkArgs = {
  vs_id: Scalars['bigint'];
};

/** Boolean expression to compare columns of type "time". All fields are combined with logical 'AND'. */
export type Time_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['time']>;
  _gt?: InputMaybe<Scalars['time']>;
  _gte?: InputMaybe<Scalars['time']>;
  _in?: InputMaybe<Array<Scalars['time']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['time']>;
  _lte?: InputMaybe<Scalars['time']>;
  _neq?: InputMaybe<Scalars['time']>;
  _nin?: InputMaybe<Array<Scalars['time']>>;
};

/** Boolean expression to compare columns of type "timestamptz". All fields are combined with logical 'AND'. */
export type Timestamptz_Comparison_Exp = {
  _eq?: InputMaybe<Scalars['timestamptz']>;
  _gt?: InputMaybe<Scalars['timestamptz']>;
  _gte?: InputMaybe<Scalars['timestamptz']>;
  _in?: InputMaybe<Array<Scalars['timestamptz']>>;
  _is_null?: InputMaybe<Scalars['Boolean']>;
  _lt?: InputMaybe<Scalars['timestamptz']>;
  _lte?: InputMaybe<Scalars['timestamptz']>;
  _neq?: InputMaybe<Scalars['timestamptz']>;
  _nin?: InputMaybe<Array<Scalars['timestamptz']>>;
};

/** columns and relationships of "upozorneni" */
export type Upozorneni = {
  __typename?: 'upozorneni';
  up_barvy: Scalars['bigint'];
  up_id: Scalars['bigint'];
  up_kdo: Scalars['bigint'];
  up_lock: Scalars['Boolean'];
  up_nadpis: Scalars['String'];
  up_text: Scalars['String'];
  up_timestamp?: Maybe<Scalars['timestamptz']>;
  up_timestamp_add: Scalars['timestamptz'];
  /** An array relationship */
  upozorneni_skupinies: Array<Upozorneni_Skupiny>;
  /** An aggregate relationship */
  upozorneni_skupinies_aggregate: Upozorneni_Skupiny_Aggregate;
  /** An object relationship */
  user: Users;
};


/** columns and relationships of "upozorneni" */
export type UpozorneniUpozorneni_SkupiniesArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};


/** columns and relationships of "upozorneni" */
export type UpozorneniUpozorneni_Skupinies_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Skupiny_Order_By>>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};

/** aggregated selection of "upozorneni" */
export type Upozorneni_Aggregate = {
  __typename?: 'upozorneni_aggregate';
  aggregate?: Maybe<Upozorneni_Aggregate_Fields>;
  nodes: Array<Upozorneni>;
};

/** aggregate fields of "upozorneni" */
export type Upozorneni_Aggregate_Fields = {
  __typename?: 'upozorneni_aggregate_fields';
  avg?: Maybe<Upozorneni_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Upozorneni_Max_Fields>;
  min?: Maybe<Upozorneni_Min_Fields>;
  stddev?: Maybe<Upozorneni_Stddev_Fields>;
  stddev_pop?: Maybe<Upozorneni_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Upozorneni_Stddev_Samp_Fields>;
  sum?: Maybe<Upozorneni_Sum_Fields>;
  var_pop?: Maybe<Upozorneni_Var_Pop_Fields>;
  var_samp?: Maybe<Upozorneni_Var_Samp_Fields>;
  variance?: Maybe<Upozorneni_Variance_Fields>;
};


/** aggregate fields of "upozorneni" */
export type Upozorneni_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Upozorneni_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "upozorneni" */
export type Upozorneni_Aggregate_Order_By = {
  avg?: InputMaybe<Upozorneni_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Upozorneni_Max_Order_By>;
  min?: InputMaybe<Upozorneni_Min_Order_By>;
  stddev?: InputMaybe<Upozorneni_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Upozorneni_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Upozorneni_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Upozorneni_Sum_Order_By>;
  var_pop?: InputMaybe<Upozorneni_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Upozorneni_Var_Samp_Order_By>;
  variance?: InputMaybe<Upozorneni_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "upozorneni" */
export type Upozorneni_Arr_Rel_Insert_Input = {
  data: Array<Upozorneni_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Upozorneni_On_Conflict>;
};

/** aggregate avg on columns */
export type Upozorneni_Avg_Fields = {
  __typename?: 'upozorneni_avg_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "upozorneni" */
export type Upozorneni_Avg_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "upozorneni". All fields are combined with a logical 'AND'. */
export type Upozorneni_Bool_Exp = {
  _and?: InputMaybe<Array<Upozorneni_Bool_Exp>>;
  _not?: InputMaybe<Upozorneni_Bool_Exp>;
  _or?: InputMaybe<Array<Upozorneni_Bool_Exp>>;
  up_barvy?: InputMaybe<Bigint_Comparison_Exp>;
  up_id?: InputMaybe<Bigint_Comparison_Exp>;
  up_kdo?: InputMaybe<Bigint_Comparison_Exp>;
  up_lock?: InputMaybe<Boolean_Comparison_Exp>;
  up_nadpis?: InputMaybe<String_Comparison_Exp>;
  up_text?: InputMaybe<String_Comparison_Exp>;
  up_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  up_timestamp_add?: InputMaybe<Timestamptz_Comparison_Exp>;
  upozorneni_skupinies?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
  user?: InputMaybe<Users_Bool_Exp>;
};

/** unique or primary key constraints on table "upozorneni" */
export enum Upozorneni_Constraint {
  /** unique or primary key constraint */
  Idx_24765Primary = 'idx_24765_primary'
}

/** input type for incrementing numeric columns in table "upozorneni" */
export type Upozorneni_Inc_Input = {
  up_barvy?: InputMaybe<Scalars['bigint']>;
  up_id?: InputMaybe<Scalars['bigint']>;
  up_kdo?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "upozorneni" */
export type Upozorneni_Insert_Input = {
  up_barvy?: InputMaybe<Scalars['bigint']>;
  up_id?: InputMaybe<Scalars['bigint']>;
  up_kdo?: InputMaybe<Scalars['bigint']>;
  up_lock?: InputMaybe<Scalars['Boolean']>;
  up_nadpis?: InputMaybe<Scalars['String']>;
  up_text?: InputMaybe<Scalars['String']>;
  up_timestamp?: InputMaybe<Scalars['timestamptz']>;
  up_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
  upozorneni_skupinies?: InputMaybe<Upozorneni_Skupiny_Arr_Rel_Insert_Input>;
  user?: InputMaybe<Users_Obj_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Upozorneni_Max_Fields = {
  __typename?: 'upozorneni_max_fields';
  up_barvy?: Maybe<Scalars['bigint']>;
  up_id?: Maybe<Scalars['bigint']>;
  up_kdo?: Maybe<Scalars['bigint']>;
  up_nadpis?: Maybe<Scalars['String']>;
  up_text?: Maybe<Scalars['String']>;
  up_timestamp?: Maybe<Scalars['timestamptz']>;
  up_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "upozorneni" */
export type Upozorneni_Max_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
  up_nadpis?: InputMaybe<Order_By>;
  up_text?: InputMaybe<Order_By>;
  up_timestamp?: InputMaybe<Order_By>;
  up_timestamp_add?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Upozorneni_Min_Fields = {
  __typename?: 'upozorneni_min_fields';
  up_barvy?: Maybe<Scalars['bigint']>;
  up_id?: Maybe<Scalars['bigint']>;
  up_kdo?: Maybe<Scalars['bigint']>;
  up_nadpis?: Maybe<Scalars['String']>;
  up_text?: Maybe<Scalars['String']>;
  up_timestamp?: Maybe<Scalars['timestamptz']>;
  up_timestamp_add?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "upozorneni" */
export type Upozorneni_Min_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
  up_nadpis?: InputMaybe<Order_By>;
  up_text?: InputMaybe<Order_By>;
  up_timestamp?: InputMaybe<Order_By>;
  up_timestamp_add?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "upozorneni" */
export type Upozorneni_Mutation_Response = {
  __typename?: 'upozorneni_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Upozorneni>;
};

/** input type for inserting object relation for remote table "upozorneni" */
export type Upozorneni_Obj_Rel_Insert_Input = {
  data: Upozorneni_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Upozorneni_On_Conflict>;
};

/** on conflict condition type for table "upozorneni" */
export type Upozorneni_On_Conflict = {
  constraint: Upozorneni_Constraint;
  update_columns?: Array<Upozorneni_Update_Column>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};

/** Ordering options when selecting data from "upozorneni". */
export type Upozorneni_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
  up_lock?: InputMaybe<Order_By>;
  up_nadpis?: InputMaybe<Order_By>;
  up_text?: InputMaybe<Order_By>;
  up_timestamp?: InputMaybe<Order_By>;
  up_timestamp_add?: InputMaybe<Order_By>;
  upozorneni_skupinies_aggregate?: InputMaybe<Upozorneni_Skupiny_Aggregate_Order_By>;
  user?: InputMaybe<Users_Order_By>;
};

/** primary key columns input for table: upozorneni */
export type Upozorneni_Pk_Columns_Input = {
  up_id: Scalars['bigint'];
};

/** select columns of table "upozorneni" */
export enum Upozorneni_Select_Column {
  /** column name */
  UpBarvy = 'up_barvy',
  /** column name */
  UpId = 'up_id',
  /** column name */
  UpKdo = 'up_kdo',
  /** column name */
  UpLock = 'up_lock',
  /** column name */
  UpNadpis = 'up_nadpis',
  /** column name */
  UpText = 'up_text',
  /** column name */
  UpTimestamp = 'up_timestamp',
  /** column name */
  UpTimestampAdd = 'up_timestamp_add'
}

/** input type for updating data in table "upozorneni" */
export type Upozorneni_Set_Input = {
  up_barvy?: InputMaybe<Scalars['bigint']>;
  up_id?: InputMaybe<Scalars['bigint']>;
  up_kdo?: InputMaybe<Scalars['bigint']>;
  up_lock?: InputMaybe<Scalars['Boolean']>;
  up_nadpis?: InputMaybe<Scalars['String']>;
  up_text?: InputMaybe<Scalars['String']>;
  up_timestamp?: InputMaybe<Scalars['timestamptz']>;
  up_timestamp_add?: InputMaybe<Scalars['timestamptz']>;
};

/** columns and relationships of "upozorneni_skupiny" */
export type Upozorneni_Skupiny = {
  __typename?: 'upozorneni_skupiny';
  /** An object relationship */
  skupiny: Skupiny;
  /** An object relationship */
  upozorneni: Upozorneni;
  ups_color: Scalars['String'];
  ups_id: Scalars['bigint'];
  ups_id_rodic: Scalars['bigint'];
  ups_id_skupina: Scalars['bigint'];
  ups_popis: Scalars['String'];
};

/** aggregated selection of "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Aggregate = {
  __typename?: 'upozorneni_skupiny_aggregate';
  aggregate?: Maybe<Upozorneni_Skupiny_Aggregate_Fields>;
  nodes: Array<Upozorneni_Skupiny>;
};

/** aggregate fields of "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Aggregate_Fields = {
  __typename?: 'upozorneni_skupiny_aggregate_fields';
  avg?: Maybe<Upozorneni_Skupiny_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Upozorneni_Skupiny_Max_Fields>;
  min?: Maybe<Upozorneni_Skupiny_Min_Fields>;
  stddev?: Maybe<Upozorneni_Skupiny_Stddev_Fields>;
  stddev_pop?: Maybe<Upozorneni_Skupiny_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Upozorneni_Skupiny_Stddev_Samp_Fields>;
  sum?: Maybe<Upozorneni_Skupiny_Sum_Fields>;
  var_pop?: Maybe<Upozorneni_Skupiny_Var_Pop_Fields>;
  var_samp?: Maybe<Upozorneni_Skupiny_Var_Samp_Fields>;
  variance?: Maybe<Upozorneni_Skupiny_Variance_Fields>;
};


/** aggregate fields of "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Upozorneni_Skupiny_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Aggregate_Order_By = {
  avg?: InputMaybe<Upozorneni_Skupiny_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Upozorneni_Skupiny_Max_Order_By>;
  min?: InputMaybe<Upozorneni_Skupiny_Min_Order_By>;
  stddev?: InputMaybe<Upozorneni_Skupiny_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Upozorneni_Skupiny_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Upozorneni_Skupiny_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Upozorneni_Skupiny_Sum_Order_By>;
  var_pop?: InputMaybe<Upozorneni_Skupiny_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Upozorneni_Skupiny_Var_Samp_Order_By>;
  variance?: InputMaybe<Upozorneni_Skupiny_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Arr_Rel_Insert_Input = {
  data: Array<Upozorneni_Skupiny_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Upozorneni_Skupiny_On_Conflict>;
};

/** aggregate avg on columns */
export type Upozorneni_Skupiny_Avg_Fields = {
  __typename?: 'upozorneni_skupiny_avg_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Avg_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "upozorneni_skupiny". All fields are combined with a logical 'AND'. */
export type Upozorneni_Skupiny_Bool_Exp = {
  _and?: InputMaybe<Array<Upozorneni_Skupiny_Bool_Exp>>;
  _not?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
  _or?: InputMaybe<Array<Upozorneni_Skupiny_Bool_Exp>>;
  skupiny?: InputMaybe<Skupiny_Bool_Exp>;
  upozorneni?: InputMaybe<Upozorneni_Bool_Exp>;
  ups_color?: InputMaybe<String_Comparison_Exp>;
  ups_id?: InputMaybe<Bigint_Comparison_Exp>;
  ups_id_rodic?: InputMaybe<Bigint_Comparison_Exp>;
  ups_id_skupina?: InputMaybe<Bigint_Comparison_Exp>;
  ups_popis?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "upozorneni_skupiny" */
export enum Upozorneni_Skupiny_Constraint {
  /** unique or primary key constraint */
  Idx_24777Primary = 'idx_24777_primary'
}

/** input type for incrementing numeric columns in table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Inc_Input = {
  ups_id?: InputMaybe<Scalars['bigint']>;
  ups_id_rodic?: InputMaybe<Scalars['bigint']>;
  ups_id_skupina?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Insert_Input = {
  skupiny?: InputMaybe<Skupiny_Obj_Rel_Insert_Input>;
  upozorneni?: InputMaybe<Upozorneni_Obj_Rel_Insert_Input>;
  ups_color?: InputMaybe<Scalars['String']>;
  ups_id?: InputMaybe<Scalars['bigint']>;
  ups_id_rodic?: InputMaybe<Scalars['bigint']>;
  ups_id_skupina?: InputMaybe<Scalars['bigint']>;
  ups_popis?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Upozorneni_Skupiny_Max_Fields = {
  __typename?: 'upozorneni_skupiny_max_fields';
  ups_color?: Maybe<Scalars['String']>;
  ups_id?: Maybe<Scalars['bigint']>;
  ups_id_rodic?: Maybe<Scalars['bigint']>;
  ups_id_skupina?: Maybe<Scalars['bigint']>;
  ups_popis?: Maybe<Scalars['String']>;
};

/** order by max() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Max_Order_By = {
  ups_color?: InputMaybe<Order_By>;
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
  ups_popis?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Upozorneni_Skupiny_Min_Fields = {
  __typename?: 'upozorneni_skupiny_min_fields';
  ups_color?: Maybe<Scalars['String']>;
  ups_id?: Maybe<Scalars['bigint']>;
  ups_id_rodic?: Maybe<Scalars['bigint']>;
  ups_id_skupina?: Maybe<Scalars['bigint']>;
  ups_popis?: Maybe<Scalars['String']>;
};

/** order by min() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Min_Order_By = {
  ups_color?: InputMaybe<Order_By>;
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
  ups_popis?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Mutation_Response = {
  __typename?: 'upozorneni_skupiny_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Upozorneni_Skupiny>;
};

/** on conflict condition type for table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_On_Conflict = {
  constraint: Upozorneni_Skupiny_Constraint;
  update_columns?: Array<Upozorneni_Skupiny_Update_Column>;
  where?: InputMaybe<Upozorneni_Skupiny_Bool_Exp>;
};

/** Ordering options when selecting data from "upozorneni_skupiny". */
export type Upozorneni_Skupiny_Order_By = {
  skupiny?: InputMaybe<Skupiny_Order_By>;
  upozorneni?: InputMaybe<Upozorneni_Order_By>;
  ups_color?: InputMaybe<Order_By>;
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
  ups_popis?: InputMaybe<Order_By>;
};

/** primary key columns input for table: upozorneni_skupiny */
export type Upozorneni_Skupiny_Pk_Columns_Input = {
  ups_id: Scalars['bigint'];
};

/** select columns of table "upozorneni_skupiny" */
export enum Upozorneni_Skupiny_Select_Column {
  /** column name */
  UpsColor = 'ups_color',
  /** column name */
  UpsId = 'ups_id',
  /** column name */
  UpsIdRodic = 'ups_id_rodic',
  /** column name */
  UpsIdSkupina = 'ups_id_skupina',
  /** column name */
  UpsPopis = 'ups_popis'
}

/** input type for updating data in table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Set_Input = {
  ups_color?: InputMaybe<Scalars['String']>;
  ups_id?: InputMaybe<Scalars['bigint']>;
  ups_id_rodic?: InputMaybe<Scalars['bigint']>;
  ups_id_skupina?: InputMaybe<Scalars['bigint']>;
  ups_popis?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Upozorneni_Skupiny_Stddev_Fields = {
  __typename?: 'upozorneni_skupiny_stddev_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Stddev_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Upozorneni_Skupiny_Stddev_Pop_Fields = {
  __typename?: 'upozorneni_skupiny_stddev_pop_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Stddev_Pop_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Upozorneni_Skupiny_Stddev_Samp_Fields = {
  __typename?: 'upozorneni_skupiny_stddev_samp_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Stddev_Samp_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Upozorneni_Skupiny_Sum_Fields = {
  __typename?: 'upozorneni_skupiny_sum_fields';
  ups_id?: Maybe<Scalars['bigint']>;
  ups_id_rodic?: Maybe<Scalars['bigint']>;
  ups_id_skupina?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Sum_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** update columns of table "upozorneni_skupiny" */
export enum Upozorneni_Skupiny_Update_Column {
  /** column name */
  UpsColor = 'ups_color',
  /** column name */
  UpsId = 'ups_id',
  /** column name */
  UpsIdRodic = 'ups_id_rodic',
  /** column name */
  UpsIdSkupina = 'ups_id_skupina',
  /** column name */
  UpsPopis = 'ups_popis'
}

/** aggregate var_pop on columns */
export type Upozorneni_Skupiny_Var_Pop_Fields = {
  __typename?: 'upozorneni_skupiny_var_pop_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Var_Pop_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Upozorneni_Skupiny_Var_Samp_Fields = {
  __typename?: 'upozorneni_skupiny_var_samp_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Var_Samp_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Upozorneni_Skupiny_Variance_Fields = {
  __typename?: 'upozorneni_skupiny_variance_fields';
  ups_id?: Maybe<Scalars['Float']>;
  ups_id_rodic?: Maybe<Scalars['Float']>;
  ups_id_skupina?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "upozorneni_skupiny" */
export type Upozorneni_Skupiny_Variance_Order_By = {
  ups_id?: InputMaybe<Order_By>;
  ups_id_rodic?: InputMaybe<Order_By>;
  ups_id_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev on columns */
export type Upozorneni_Stddev_Fields = {
  __typename?: 'upozorneni_stddev_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "upozorneni" */
export type Upozorneni_Stddev_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Upozorneni_Stddev_Pop_Fields = {
  __typename?: 'upozorneni_stddev_pop_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "upozorneni" */
export type Upozorneni_Stddev_Pop_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Upozorneni_Stddev_Samp_Fields = {
  __typename?: 'upozorneni_stddev_samp_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "upozorneni" */
export type Upozorneni_Stddev_Samp_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Upozorneni_Sum_Fields = {
  __typename?: 'upozorneni_sum_fields';
  up_barvy?: Maybe<Scalars['bigint']>;
  up_id?: Maybe<Scalars['bigint']>;
  up_kdo?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "upozorneni" */
export type Upozorneni_Sum_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** update columns of table "upozorneni" */
export enum Upozorneni_Update_Column {
  /** column name */
  UpBarvy = 'up_barvy',
  /** column name */
  UpId = 'up_id',
  /** column name */
  UpKdo = 'up_kdo',
  /** column name */
  UpLock = 'up_lock',
  /** column name */
  UpNadpis = 'up_nadpis',
  /** column name */
  UpText = 'up_text',
  /** column name */
  UpTimestamp = 'up_timestamp',
  /** column name */
  UpTimestampAdd = 'up_timestamp_add'
}

/** aggregate var_pop on columns */
export type Upozorneni_Var_Pop_Fields = {
  __typename?: 'upozorneni_var_pop_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "upozorneni" */
export type Upozorneni_Var_Pop_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Upozorneni_Var_Samp_Fields = {
  __typename?: 'upozorneni_var_samp_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "upozorneni" */
export type Upozorneni_Var_Samp_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Upozorneni_Variance_Fields = {
  __typename?: 'upozorneni_variance_fields';
  up_barvy?: Maybe<Scalars['Float']>;
  up_id?: Maybe<Scalars['Float']>;
  up_kdo?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "upozorneni" */
export type Upozorneni_Variance_Order_By = {
  up_barvy?: InputMaybe<Order_By>;
  up_id?: InputMaybe<Order_By>;
  up_kdo?: InputMaybe<Order_By>;
};

/** columns and relationships of "users" */
export type Users = {
  __typename?: 'users';
  /** An array relationship */
  akce_items: Array<Akce_Item>;
  /** An aggregate relationship */
  akce_items_aggregate: Akce_Item_Aggregate;
  /** An array relationship */
  aktualities: Array<Aktuality>;
  /** An aggregate relationship */
  aktualities_aggregate: Aktuality_Aggregate;
  /** An array relationship */
  dokumenties: Array<Dokumenty>;
  /** An aggregate relationship */
  dokumenties_aggregate: Dokumenty_Aggregate;
  /** An array relationship */
  galerie_fotos: Array<Galerie_Foto>;
  /** An aggregate relationship */
  galerie_fotos_aggregate: Galerie_Foto_Aggregate;
  /** An array relationship */
  nabidkas: Array<Nabidka>;
  /** An aggregate relationship */
  nabidkas_aggregate: Nabidka_Aggregate;
  /** An array relationship */
  paries: Array<Pary>;
  /** An aggregate relationship */
  paries_aggregate: Pary_Aggregate;
  /** An array relationship */
  paryNavrhsByPnPartner: Array<Pary_Navrh>;
  /** An aggregate relationship */
  paryNavrhsByPnPartner_aggregate: Pary_Navrh_Aggregate;
  /** An array relationship */
  paryNavrhsByPnPartnerka: Array<Pary_Navrh>;
  /** An aggregate relationship */
  paryNavrhsByPnPartnerka_aggregate: Pary_Navrh_Aggregate;
  /** An array relationship */
  pary_navrhs: Array<Pary_Navrh>;
  /** An aggregate relationship */
  pary_navrhs_aggregate: Pary_Navrh_Aggregate;
  /** An object relationship */
  permission: Permissions;
  /** An array relationship */
  platby_items: Array<Platby_Item>;
  /** An aggregate relationship */
  platby_items_aggregate: Platby_Item_Aggregate;
  /** An array relationship */
  rozpis: Array<Rozpis>;
  /** An aggregate relationship */
  rozpis_aggregate: Rozpis_Aggregate;
  /** An object relationship */
  skupiny: Skupiny;
  u_ban: Scalars['Boolean'];
  u_city: Scalars['String'];
  u_confirmed: Scalars['Boolean'];
  u_conscription_number: Scalars['String'];
  u_created_at: Scalars['timestamptz'];
  u_dancer: Scalars['Boolean'];
  u_district: Scalars['String'];
  u_email: Scalars['String'];
  u_gdpr_signed_at?: Maybe<Scalars['timestamptz']>;
  u_group: Scalars['bigint'];
  u_id: Scalars['bigint'];
  u_jmeno: Scalars['String'];
  u_level: Scalars['smallint'];
  u_lock: Scalars['Boolean'];
  u_login: Scalars['String'];
  u_member_since?: Maybe<Scalars['timestamptz']>;
  u_member_until?: Maybe<Scalars['timestamptz']>;
  u_narozeni: Scalars['date'];
  u_nationality: Scalars['String'];
  u_orientation_number: Scalars['String'];
  u_pass: Scalars['bpchar'];
  u_pohlavi: Scalars['String'];
  u_postal_code: Scalars['String'];
  u_poznamky: Scalars['String'];
  u_prijmeni: Scalars['String'];
  u_rodne_cislo?: Maybe<Scalars['String']>;
  u_skupina: Scalars['bigint'];
  u_street: Scalars['String'];
  u_system: Scalars['Boolean'];
  u_teacher: Scalars['Boolean'];
  u_telefon: Scalars['String'];
  u_timestamp: Scalars['timestamptz'];
  /** An array relationship */
  upozornenis: Array<Upozorneni>;
  /** An aggregate relationship */
  upozornenis_aggregate: Upozorneni_Aggregate;
};


/** columns and relationships of "users" */
export type UsersAkce_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersAkce_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Akce_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Akce_Item_Order_By>>;
  where?: InputMaybe<Akce_Item_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersAktualitiesArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersAktualities_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Aktuality_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Aktuality_Order_By>>;
  where?: InputMaybe<Aktuality_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersDokumentiesArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersDokumenties_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Dokumenty_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Dokumenty_Order_By>>;
  where?: InputMaybe<Dokumenty_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersGalerie_FotosArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersGalerie_Fotos_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Galerie_Foto_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Galerie_Foto_Order_By>>;
  where?: InputMaybe<Galerie_Foto_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersNabidkasArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersNabidkas_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Nabidka_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Nabidka_Order_By>>;
  where?: InputMaybe<Nabidka_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersPariesArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersParies_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Order_By>>;
  where?: InputMaybe<Pary_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersParyNavrhsByPnPartnerArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersParyNavrhsByPnPartner_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersParyNavrhsByPnPartnerkaArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersParyNavrhsByPnPartnerka_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersPary_NavrhsArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersPary_Navrhs_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Pary_Navrh_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Pary_Navrh_Order_By>>;
  where?: InputMaybe<Pary_Navrh_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersPlatby_ItemsArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersPlatby_Items_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Platby_Item_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Platby_Item_Order_By>>;
  where?: InputMaybe<Platby_Item_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersRozpisArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersRozpis_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Rozpis_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Rozpis_Order_By>>;
  where?: InputMaybe<Rozpis_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersUpozornenisArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};


/** columns and relationships of "users" */
export type UsersUpozornenis_AggregateArgs = {
  distinct_on?: InputMaybe<Array<Upozorneni_Select_Column>>;
  limit?: InputMaybe<Scalars['Int']>;
  offset?: InputMaybe<Scalars['Int']>;
  order_by?: InputMaybe<Array<Upozorneni_Order_By>>;
  where?: InputMaybe<Upozorneni_Bool_Exp>;
};

/** aggregated selection of "users" */
export type Users_Aggregate = {
  __typename?: 'users_aggregate';
  aggregate?: Maybe<Users_Aggregate_Fields>;
  nodes: Array<Users>;
};

/** aggregate fields of "users" */
export type Users_Aggregate_Fields = {
  __typename?: 'users_aggregate_fields';
  avg?: Maybe<Users_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Users_Max_Fields>;
  min?: Maybe<Users_Min_Fields>;
  stddev?: Maybe<Users_Stddev_Fields>;
  stddev_pop?: Maybe<Users_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Users_Stddev_Samp_Fields>;
  sum?: Maybe<Users_Sum_Fields>;
  var_pop?: Maybe<Users_Var_Pop_Fields>;
  var_samp?: Maybe<Users_Var_Samp_Fields>;
  variance?: Maybe<Users_Variance_Fields>;
};


/** aggregate fields of "users" */
export type Users_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Users_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** order by aggregate values of table "users" */
export type Users_Aggregate_Order_By = {
  avg?: InputMaybe<Users_Avg_Order_By>;
  count?: InputMaybe<Order_By>;
  max?: InputMaybe<Users_Max_Order_By>;
  min?: InputMaybe<Users_Min_Order_By>;
  stddev?: InputMaybe<Users_Stddev_Order_By>;
  stddev_pop?: InputMaybe<Users_Stddev_Pop_Order_By>;
  stddev_samp?: InputMaybe<Users_Stddev_Samp_Order_By>;
  sum?: InputMaybe<Users_Sum_Order_By>;
  var_pop?: InputMaybe<Users_Var_Pop_Order_By>;
  var_samp?: InputMaybe<Users_Var_Samp_Order_By>;
  variance?: InputMaybe<Users_Variance_Order_By>;
};

/** input type for inserting array relation for remote table "users" */
export type Users_Arr_Rel_Insert_Input = {
  data: Array<Users_Insert_Input>;
  /** on conflict condition */
  on_conflict?: InputMaybe<Users_On_Conflict>;
};

/** aggregate avg on columns */
export type Users_Avg_Fields = {
  __typename?: 'users_avg_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by avg() on columns of table "users" */
export type Users_Avg_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** Boolean expression to filter rows from the table "users". All fields are combined with a logical 'AND'. */
export type Users_Bool_Exp = {
  _and?: InputMaybe<Array<Users_Bool_Exp>>;
  _not?: InputMaybe<Users_Bool_Exp>;
  _or?: InputMaybe<Array<Users_Bool_Exp>>;
  akce_items?: InputMaybe<Akce_Item_Bool_Exp>;
  aktualities?: InputMaybe<Aktuality_Bool_Exp>;
  dokumenties?: InputMaybe<Dokumenty_Bool_Exp>;
  galerie_fotos?: InputMaybe<Galerie_Foto_Bool_Exp>;
  nabidkas?: InputMaybe<Nabidka_Bool_Exp>;
  paries?: InputMaybe<Pary_Bool_Exp>;
  paryNavrhsByPnPartner?: InputMaybe<Pary_Navrh_Bool_Exp>;
  paryNavrhsByPnPartnerka?: InputMaybe<Pary_Navrh_Bool_Exp>;
  pary_navrhs?: InputMaybe<Pary_Navrh_Bool_Exp>;
  permission?: InputMaybe<Permissions_Bool_Exp>;
  platby_items?: InputMaybe<Platby_Item_Bool_Exp>;
  rozpis?: InputMaybe<Rozpis_Bool_Exp>;
  skupiny?: InputMaybe<Skupiny_Bool_Exp>;
  u_ban?: InputMaybe<Boolean_Comparison_Exp>;
  u_city?: InputMaybe<String_Comparison_Exp>;
  u_confirmed?: InputMaybe<Boolean_Comparison_Exp>;
  u_conscription_number?: InputMaybe<String_Comparison_Exp>;
  u_created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  u_dancer?: InputMaybe<Boolean_Comparison_Exp>;
  u_district?: InputMaybe<String_Comparison_Exp>;
  u_email?: InputMaybe<String_Comparison_Exp>;
  u_gdpr_signed_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  u_group?: InputMaybe<Bigint_Comparison_Exp>;
  u_id?: InputMaybe<Bigint_Comparison_Exp>;
  u_jmeno?: InputMaybe<String_Comparison_Exp>;
  u_level?: InputMaybe<Smallint_Comparison_Exp>;
  u_lock?: InputMaybe<Boolean_Comparison_Exp>;
  u_login?: InputMaybe<String_Comparison_Exp>;
  u_member_since?: InputMaybe<Timestamptz_Comparison_Exp>;
  u_member_until?: InputMaybe<Timestamptz_Comparison_Exp>;
  u_narozeni?: InputMaybe<Date_Comparison_Exp>;
  u_nationality?: InputMaybe<String_Comparison_Exp>;
  u_orientation_number?: InputMaybe<String_Comparison_Exp>;
  u_pass?: InputMaybe<Bpchar_Comparison_Exp>;
  u_pohlavi?: InputMaybe<String_Comparison_Exp>;
  u_postal_code?: InputMaybe<String_Comparison_Exp>;
  u_poznamky?: InputMaybe<String_Comparison_Exp>;
  u_prijmeni?: InputMaybe<String_Comparison_Exp>;
  u_rodne_cislo?: InputMaybe<String_Comparison_Exp>;
  u_skupina?: InputMaybe<Bigint_Comparison_Exp>;
  u_street?: InputMaybe<String_Comparison_Exp>;
  u_system?: InputMaybe<Boolean_Comparison_Exp>;
  u_teacher?: InputMaybe<Boolean_Comparison_Exp>;
  u_telefon?: InputMaybe<String_Comparison_Exp>;
  u_timestamp?: InputMaybe<Timestamptz_Comparison_Exp>;
  upozornenis?: InputMaybe<Upozorneni_Bool_Exp>;
};

/** unique or primary key constraints on table "users" */
export enum Users_Constraint {
  /** unique or primary key constraint */
  Idx_24786Primary = 'idx_24786_primary',
  /** unique or primary key constraint */
  Idx_24786ULogin = 'idx_24786_u_login'
}

/** input type for incrementing numeric columns in table "users" */
export type Users_Inc_Input = {
  u_group?: InputMaybe<Scalars['bigint']>;
  u_id?: InputMaybe<Scalars['bigint']>;
  u_level?: InputMaybe<Scalars['smallint']>;
  u_skupina?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "users" */
export type Users_Insert_Input = {
  akce_items?: InputMaybe<Akce_Item_Arr_Rel_Insert_Input>;
  aktualities?: InputMaybe<Aktuality_Arr_Rel_Insert_Input>;
  dokumenties?: InputMaybe<Dokumenty_Arr_Rel_Insert_Input>;
  galerie_fotos?: InputMaybe<Galerie_Foto_Arr_Rel_Insert_Input>;
  nabidkas?: InputMaybe<Nabidka_Arr_Rel_Insert_Input>;
  paries?: InputMaybe<Pary_Arr_Rel_Insert_Input>;
  paryNavrhsByPnPartner?: InputMaybe<Pary_Navrh_Arr_Rel_Insert_Input>;
  paryNavrhsByPnPartnerka?: InputMaybe<Pary_Navrh_Arr_Rel_Insert_Input>;
  pary_navrhs?: InputMaybe<Pary_Navrh_Arr_Rel_Insert_Input>;
  permission?: InputMaybe<Permissions_Obj_Rel_Insert_Input>;
  platby_items?: InputMaybe<Platby_Item_Arr_Rel_Insert_Input>;
  rozpis?: InputMaybe<Rozpis_Arr_Rel_Insert_Input>;
  skupiny?: InputMaybe<Skupiny_Obj_Rel_Insert_Input>;
  u_ban?: InputMaybe<Scalars['Boolean']>;
  u_city?: InputMaybe<Scalars['String']>;
  u_confirmed?: InputMaybe<Scalars['Boolean']>;
  u_conscription_number?: InputMaybe<Scalars['String']>;
  u_created_at?: InputMaybe<Scalars['timestamptz']>;
  u_dancer?: InputMaybe<Scalars['Boolean']>;
  u_district?: InputMaybe<Scalars['String']>;
  u_email?: InputMaybe<Scalars['String']>;
  u_gdpr_signed_at?: InputMaybe<Scalars['timestamptz']>;
  u_group?: InputMaybe<Scalars['bigint']>;
  u_id?: InputMaybe<Scalars['bigint']>;
  u_jmeno?: InputMaybe<Scalars['String']>;
  u_level?: InputMaybe<Scalars['smallint']>;
  u_lock?: InputMaybe<Scalars['Boolean']>;
  u_login?: InputMaybe<Scalars['String']>;
  u_member_since?: InputMaybe<Scalars['timestamptz']>;
  u_member_until?: InputMaybe<Scalars['timestamptz']>;
  u_narozeni?: InputMaybe<Scalars['date']>;
  u_nationality?: InputMaybe<Scalars['String']>;
  u_orientation_number?: InputMaybe<Scalars['String']>;
  u_pass?: InputMaybe<Scalars['bpchar']>;
  u_pohlavi?: InputMaybe<Scalars['String']>;
  u_postal_code?: InputMaybe<Scalars['String']>;
  u_poznamky?: InputMaybe<Scalars['String']>;
  u_prijmeni?: InputMaybe<Scalars['String']>;
  u_rodne_cislo?: InputMaybe<Scalars['String']>;
  u_skupina?: InputMaybe<Scalars['bigint']>;
  u_street?: InputMaybe<Scalars['String']>;
  u_system?: InputMaybe<Scalars['Boolean']>;
  u_teacher?: InputMaybe<Scalars['Boolean']>;
  u_telefon?: InputMaybe<Scalars['String']>;
  u_timestamp?: InputMaybe<Scalars['timestamptz']>;
  upozornenis?: InputMaybe<Upozorneni_Arr_Rel_Insert_Input>;
};

/** aggregate max on columns */
export type Users_Max_Fields = {
  __typename?: 'users_max_fields';
  u_city?: Maybe<Scalars['String']>;
  u_conscription_number?: Maybe<Scalars['String']>;
  u_created_at?: Maybe<Scalars['timestamptz']>;
  u_district?: Maybe<Scalars['String']>;
  u_email?: Maybe<Scalars['String']>;
  u_gdpr_signed_at?: Maybe<Scalars['timestamptz']>;
  u_group?: Maybe<Scalars['bigint']>;
  u_id?: Maybe<Scalars['bigint']>;
  u_jmeno?: Maybe<Scalars['String']>;
  u_level?: Maybe<Scalars['smallint']>;
  u_login?: Maybe<Scalars['String']>;
  u_member_since?: Maybe<Scalars['timestamptz']>;
  u_member_until?: Maybe<Scalars['timestamptz']>;
  u_narozeni?: Maybe<Scalars['date']>;
  u_nationality?: Maybe<Scalars['String']>;
  u_orientation_number?: Maybe<Scalars['String']>;
  u_pass?: Maybe<Scalars['bpchar']>;
  u_pohlavi?: Maybe<Scalars['String']>;
  u_postal_code?: Maybe<Scalars['String']>;
  u_poznamky?: Maybe<Scalars['String']>;
  u_prijmeni?: Maybe<Scalars['String']>;
  u_rodne_cislo?: Maybe<Scalars['String']>;
  u_skupina?: Maybe<Scalars['bigint']>;
  u_street?: Maybe<Scalars['String']>;
  u_telefon?: Maybe<Scalars['String']>;
  u_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by max() on columns of table "users" */
export type Users_Max_Order_By = {
  u_city?: InputMaybe<Order_By>;
  u_conscription_number?: InputMaybe<Order_By>;
  u_created_at?: InputMaybe<Order_By>;
  u_district?: InputMaybe<Order_By>;
  u_email?: InputMaybe<Order_By>;
  u_gdpr_signed_at?: InputMaybe<Order_By>;
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_jmeno?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_login?: InputMaybe<Order_By>;
  u_member_since?: InputMaybe<Order_By>;
  u_member_until?: InputMaybe<Order_By>;
  u_narozeni?: InputMaybe<Order_By>;
  u_nationality?: InputMaybe<Order_By>;
  u_orientation_number?: InputMaybe<Order_By>;
  u_pass?: InputMaybe<Order_By>;
  u_pohlavi?: InputMaybe<Order_By>;
  u_postal_code?: InputMaybe<Order_By>;
  u_poznamky?: InputMaybe<Order_By>;
  u_prijmeni?: InputMaybe<Order_By>;
  u_rodne_cislo?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
  u_street?: InputMaybe<Order_By>;
  u_telefon?: InputMaybe<Order_By>;
  u_timestamp?: InputMaybe<Order_By>;
};

/** aggregate min on columns */
export type Users_Min_Fields = {
  __typename?: 'users_min_fields';
  u_city?: Maybe<Scalars['String']>;
  u_conscription_number?: Maybe<Scalars['String']>;
  u_created_at?: Maybe<Scalars['timestamptz']>;
  u_district?: Maybe<Scalars['String']>;
  u_email?: Maybe<Scalars['String']>;
  u_gdpr_signed_at?: Maybe<Scalars['timestamptz']>;
  u_group?: Maybe<Scalars['bigint']>;
  u_id?: Maybe<Scalars['bigint']>;
  u_jmeno?: Maybe<Scalars['String']>;
  u_level?: Maybe<Scalars['smallint']>;
  u_login?: Maybe<Scalars['String']>;
  u_member_since?: Maybe<Scalars['timestamptz']>;
  u_member_until?: Maybe<Scalars['timestamptz']>;
  u_narozeni?: Maybe<Scalars['date']>;
  u_nationality?: Maybe<Scalars['String']>;
  u_orientation_number?: Maybe<Scalars['String']>;
  u_pass?: Maybe<Scalars['bpchar']>;
  u_pohlavi?: Maybe<Scalars['String']>;
  u_postal_code?: Maybe<Scalars['String']>;
  u_poznamky?: Maybe<Scalars['String']>;
  u_prijmeni?: Maybe<Scalars['String']>;
  u_rodne_cislo?: Maybe<Scalars['String']>;
  u_skupina?: Maybe<Scalars['bigint']>;
  u_street?: Maybe<Scalars['String']>;
  u_telefon?: Maybe<Scalars['String']>;
  u_timestamp?: Maybe<Scalars['timestamptz']>;
};

/** order by min() on columns of table "users" */
export type Users_Min_Order_By = {
  u_city?: InputMaybe<Order_By>;
  u_conscription_number?: InputMaybe<Order_By>;
  u_created_at?: InputMaybe<Order_By>;
  u_district?: InputMaybe<Order_By>;
  u_email?: InputMaybe<Order_By>;
  u_gdpr_signed_at?: InputMaybe<Order_By>;
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_jmeno?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_login?: InputMaybe<Order_By>;
  u_member_since?: InputMaybe<Order_By>;
  u_member_until?: InputMaybe<Order_By>;
  u_narozeni?: InputMaybe<Order_By>;
  u_nationality?: InputMaybe<Order_By>;
  u_orientation_number?: InputMaybe<Order_By>;
  u_pass?: InputMaybe<Order_By>;
  u_pohlavi?: InputMaybe<Order_By>;
  u_postal_code?: InputMaybe<Order_By>;
  u_poznamky?: InputMaybe<Order_By>;
  u_prijmeni?: InputMaybe<Order_By>;
  u_rodne_cislo?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
  u_street?: InputMaybe<Order_By>;
  u_telefon?: InputMaybe<Order_By>;
  u_timestamp?: InputMaybe<Order_By>;
};

/** response of any mutation on the table "users" */
export type Users_Mutation_Response = {
  __typename?: 'users_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Users>;
};

/** input type for inserting object relation for remote table "users" */
export type Users_Obj_Rel_Insert_Input = {
  data: Users_Insert_Input;
  /** on conflict condition */
  on_conflict?: InputMaybe<Users_On_Conflict>;
};

/** on conflict condition type for table "users" */
export type Users_On_Conflict = {
  constraint: Users_Constraint;
  update_columns?: Array<Users_Update_Column>;
  where?: InputMaybe<Users_Bool_Exp>;
};

/** Ordering options when selecting data from "users". */
export type Users_Order_By = {
  akce_items_aggregate?: InputMaybe<Akce_Item_Aggregate_Order_By>;
  aktualities_aggregate?: InputMaybe<Aktuality_Aggregate_Order_By>;
  dokumenties_aggregate?: InputMaybe<Dokumenty_Aggregate_Order_By>;
  galerie_fotos_aggregate?: InputMaybe<Galerie_Foto_Aggregate_Order_By>;
  nabidkas_aggregate?: InputMaybe<Nabidka_Aggregate_Order_By>;
  paries_aggregate?: InputMaybe<Pary_Aggregate_Order_By>;
  paryNavrhsByPnPartner_aggregate?: InputMaybe<Pary_Navrh_Aggregate_Order_By>;
  paryNavrhsByPnPartnerka_aggregate?: InputMaybe<Pary_Navrh_Aggregate_Order_By>;
  pary_navrhs_aggregate?: InputMaybe<Pary_Navrh_Aggregate_Order_By>;
  permission?: InputMaybe<Permissions_Order_By>;
  platby_items_aggregate?: InputMaybe<Platby_Item_Aggregate_Order_By>;
  rozpis_aggregate?: InputMaybe<Rozpis_Aggregate_Order_By>;
  skupiny?: InputMaybe<Skupiny_Order_By>;
  u_ban?: InputMaybe<Order_By>;
  u_city?: InputMaybe<Order_By>;
  u_confirmed?: InputMaybe<Order_By>;
  u_conscription_number?: InputMaybe<Order_By>;
  u_created_at?: InputMaybe<Order_By>;
  u_dancer?: InputMaybe<Order_By>;
  u_district?: InputMaybe<Order_By>;
  u_email?: InputMaybe<Order_By>;
  u_gdpr_signed_at?: InputMaybe<Order_By>;
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_jmeno?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_lock?: InputMaybe<Order_By>;
  u_login?: InputMaybe<Order_By>;
  u_member_since?: InputMaybe<Order_By>;
  u_member_until?: InputMaybe<Order_By>;
  u_narozeni?: InputMaybe<Order_By>;
  u_nationality?: InputMaybe<Order_By>;
  u_orientation_number?: InputMaybe<Order_By>;
  u_pass?: InputMaybe<Order_By>;
  u_pohlavi?: InputMaybe<Order_By>;
  u_postal_code?: InputMaybe<Order_By>;
  u_poznamky?: InputMaybe<Order_By>;
  u_prijmeni?: InputMaybe<Order_By>;
  u_rodne_cislo?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
  u_street?: InputMaybe<Order_By>;
  u_system?: InputMaybe<Order_By>;
  u_teacher?: InputMaybe<Order_By>;
  u_telefon?: InputMaybe<Order_By>;
  u_timestamp?: InputMaybe<Order_By>;
  upozornenis_aggregate?: InputMaybe<Upozorneni_Aggregate_Order_By>;
};

/** primary key columns input for table: users */
export type Users_Pk_Columns_Input = {
  u_id: Scalars['bigint'];
};

/** select columns of table "users" */
export enum Users_Select_Column {
  /** column name */
  UBan = 'u_ban',
  /** column name */
  UCity = 'u_city',
  /** column name */
  UConfirmed = 'u_confirmed',
  /** column name */
  UConscriptionNumber = 'u_conscription_number',
  /** column name */
  UCreatedAt = 'u_created_at',
  /** column name */
  UDancer = 'u_dancer',
  /** column name */
  UDistrict = 'u_district',
  /** column name */
  UEmail = 'u_email',
  /** column name */
  UGdprSignedAt = 'u_gdpr_signed_at',
  /** column name */
  UGroup = 'u_group',
  /** column name */
  UId = 'u_id',
  /** column name */
  UJmeno = 'u_jmeno',
  /** column name */
  ULevel = 'u_level',
  /** column name */
  ULock = 'u_lock',
  /** column name */
  ULogin = 'u_login',
  /** column name */
  UMemberSince = 'u_member_since',
  /** column name */
  UMemberUntil = 'u_member_until',
  /** column name */
  UNarozeni = 'u_narozeni',
  /** column name */
  UNationality = 'u_nationality',
  /** column name */
  UOrientationNumber = 'u_orientation_number',
  /** column name */
  UPass = 'u_pass',
  /** column name */
  UPohlavi = 'u_pohlavi',
  /** column name */
  UPostalCode = 'u_postal_code',
  /** column name */
  UPoznamky = 'u_poznamky',
  /** column name */
  UPrijmeni = 'u_prijmeni',
  /** column name */
  URodneCislo = 'u_rodne_cislo',
  /** column name */
  USkupina = 'u_skupina',
  /** column name */
  UStreet = 'u_street',
  /** column name */
  USystem = 'u_system',
  /** column name */
  UTeacher = 'u_teacher',
  /** column name */
  UTelefon = 'u_telefon',
  /** column name */
  UTimestamp = 'u_timestamp'
}

/** input type for updating data in table "users" */
export type Users_Set_Input = {
  u_ban?: InputMaybe<Scalars['Boolean']>;
  u_city?: InputMaybe<Scalars['String']>;
  u_confirmed?: InputMaybe<Scalars['Boolean']>;
  u_conscription_number?: InputMaybe<Scalars['String']>;
  u_created_at?: InputMaybe<Scalars['timestamptz']>;
  u_dancer?: InputMaybe<Scalars['Boolean']>;
  u_district?: InputMaybe<Scalars['String']>;
  u_email?: InputMaybe<Scalars['String']>;
  u_gdpr_signed_at?: InputMaybe<Scalars['timestamptz']>;
  u_group?: InputMaybe<Scalars['bigint']>;
  u_id?: InputMaybe<Scalars['bigint']>;
  u_jmeno?: InputMaybe<Scalars['String']>;
  u_level?: InputMaybe<Scalars['smallint']>;
  u_lock?: InputMaybe<Scalars['Boolean']>;
  u_login?: InputMaybe<Scalars['String']>;
  u_member_since?: InputMaybe<Scalars['timestamptz']>;
  u_member_until?: InputMaybe<Scalars['timestamptz']>;
  u_narozeni?: InputMaybe<Scalars['date']>;
  u_nationality?: InputMaybe<Scalars['String']>;
  u_orientation_number?: InputMaybe<Scalars['String']>;
  u_pass?: InputMaybe<Scalars['bpchar']>;
  u_pohlavi?: InputMaybe<Scalars['String']>;
  u_postal_code?: InputMaybe<Scalars['String']>;
  u_poznamky?: InputMaybe<Scalars['String']>;
  u_prijmeni?: InputMaybe<Scalars['String']>;
  u_rodne_cislo?: InputMaybe<Scalars['String']>;
  u_skupina?: InputMaybe<Scalars['bigint']>;
  u_street?: InputMaybe<Scalars['String']>;
  u_system?: InputMaybe<Scalars['Boolean']>;
  u_teacher?: InputMaybe<Scalars['Boolean']>;
  u_telefon?: InputMaybe<Scalars['String']>;
  u_timestamp?: InputMaybe<Scalars['timestamptz']>;
};

/** columns and relationships of "users_skupiny" */
export type Users_Skupiny = {
  __typename?: 'users_skupiny';
  us_color: Scalars['String'];
  us_id: Scalars['bigint'];
  us_platba_ctvrtrok: Scalars['bigint'];
  us_platba_mesic: Scalars['bigint'];
  us_platba_pulrok: Scalars['bigint'];
  us_popis: Scalars['String'];
};

/** aggregated selection of "users_skupiny" */
export type Users_Skupiny_Aggregate = {
  __typename?: 'users_skupiny_aggregate';
  aggregate?: Maybe<Users_Skupiny_Aggregate_Fields>;
  nodes: Array<Users_Skupiny>;
};

/** aggregate fields of "users_skupiny" */
export type Users_Skupiny_Aggregate_Fields = {
  __typename?: 'users_skupiny_aggregate_fields';
  avg?: Maybe<Users_Skupiny_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Users_Skupiny_Max_Fields>;
  min?: Maybe<Users_Skupiny_Min_Fields>;
  stddev?: Maybe<Users_Skupiny_Stddev_Fields>;
  stddev_pop?: Maybe<Users_Skupiny_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Users_Skupiny_Stddev_Samp_Fields>;
  sum?: Maybe<Users_Skupiny_Sum_Fields>;
  var_pop?: Maybe<Users_Skupiny_Var_Pop_Fields>;
  var_samp?: Maybe<Users_Skupiny_Var_Samp_Fields>;
  variance?: Maybe<Users_Skupiny_Variance_Fields>;
};


/** aggregate fields of "users_skupiny" */
export type Users_Skupiny_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Users_Skupiny_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Users_Skupiny_Avg_Fields = {
  __typename?: 'users_skupiny_avg_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "users_skupiny". All fields are combined with a logical 'AND'. */
export type Users_Skupiny_Bool_Exp = {
  _and?: InputMaybe<Array<Users_Skupiny_Bool_Exp>>;
  _not?: InputMaybe<Users_Skupiny_Bool_Exp>;
  _or?: InputMaybe<Array<Users_Skupiny_Bool_Exp>>;
  us_color?: InputMaybe<String_Comparison_Exp>;
  us_id?: InputMaybe<Bigint_Comparison_Exp>;
  us_platba_ctvrtrok?: InputMaybe<Bigint_Comparison_Exp>;
  us_platba_mesic?: InputMaybe<Bigint_Comparison_Exp>;
  us_platba_pulrok?: InputMaybe<Bigint_Comparison_Exp>;
  us_popis?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "users_skupiny" */
export enum Users_Skupiny_Constraint {
  /** unique or primary key constraint */
  Idx_24808Primary = 'idx_24808_primary'
}

/** input type for incrementing numeric columns in table "users_skupiny" */
export type Users_Skupiny_Inc_Input = {
  us_id?: InputMaybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: InputMaybe<Scalars['bigint']>;
  us_platba_mesic?: InputMaybe<Scalars['bigint']>;
  us_platba_pulrok?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "users_skupiny" */
export type Users_Skupiny_Insert_Input = {
  us_color?: InputMaybe<Scalars['String']>;
  us_id?: InputMaybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: InputMaybe<Scalars['bigint']>;
  us_platba_mesic?: InputMaybe<Scalars['bigint']>;
  us_platba_pulrok?: InputMaybe<Scalars['bigint']>;
  us_popis?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Users_Skupiny_Max_Fields = {
  __typename?: 'users_skupiny_max_fields';
  us_color?: Maybe<Scalars['String']>;
  us_id?: Maybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: Maybe<Scalars['bigint']>;
  us_platba_mesic?: Maybe<Scalars['bigint']>;
  us_platba_pulrok?: Maybe<Scalars['bigint']>;
  us_popis?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Users_Skupiny_Min_Fields = {
  __typename?: 'users_skupiny_min_fields';
  us_color?: Maybe<Scalars['String']>;
  us_id?: Maybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: Maybe<Scalars['bigint']>;
  us_platba_mesic?: Maybe<Scalars['bigint']>;
  us_platba_pulrok?: Maybe<Scalars['bigint']>;
  us_popis?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "users_skupiny" */
export type Users_Skupiny_Mutation_Response = {
  __typename?: 'users_skupiny_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Users_Skupiny>;
};

/** on conflict condition type for table "users_skupiny" */
export type Users_Skupiny_On_Conflict = {
  constraint: Users_Skupiny_Constraint;
  update_columns?: Array<Users_Skupiny_Update_Column>;
  where?: InputMaybe<Users_Skupiny_Bool_Exp>;
};

/** Ordering options when selecting data from "users_skupiny". */
export type Users_Skupiny_Order_By = {
  us_color?: InputMaybe<Order_By>;
  us_id?: InputMaybe<Order_By>;
  us_platba_ctvrtrok?: InputMaybe<Order_By>;
  us_platba_mesic?: InputMaybe<Order_By>;
  us_platba_pulrok?: InputMaybe<Order_By>;
  us_popis?: InputMaybe<Order_By>;
};

/** primary key columns input for table: users_skupiny */
export type Users_Skupiny_Pk_Columns_Input = {
  us_id: Scalars['bigint'];
};

/** select columns of table "users_skupiny" */
export enum Users_Skupiny_Select_Column {
  /** column name */
  UsColor = 'us_color',
  /** column name */
  UsId = 'us_id',
  /** column name */
  UsPlatbaCtvrtrok = 'us_platba_ctvrtrok',
  /** column name */
  UsPlatbaMesic = 'us_platba_mesic',
  /** column name */
  UsPlatbaPulrok = 'us_platba_pulrok',
  /** column name */
  UsPopis = 'us_popis'
}

/** input type for updating data in table "users_skupiny" */
export type Users_Skupiny_Set_Input = {
  us_color?: InputMaybe<Scalars['String']>;
  us_id?: InputMaybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: InputMaybe<Scalars['bigint']>;
  us_platba_mesic?: InputMaybe<Scalars['bigint']>;
  us_platba_pulrok?: InputMaybe<Scalars['bigint']>;
  us_popis?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Users_Skupiny_Stddev_Fields = {
  __typename?: 'users_skupiny_stddev_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Users_Skupiny_Stddev_Pop_Fields = {
  __typename?: 'users_skupiny_stddev_pop_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Users_Skupiny_Stddev_Samp_Fields = {
  __typename?: 'users_skupiny_stddev_samp_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Users_Skupiny_Sum_Fields = {
  __typename?: 'users_skupiny_sum_fields';
  us_id?: Maybe<Scalars['bigint']>;
  us_platba_ctvrtrok?: Maybe<Scalars['bigint']>;
  us_platba_mesic?: Maybe<Scalars['bigint']>;
  us_platba_pulrok?: Maybe<Scalars['bigint']>;
};

/** update columns of table "users_skupiny" */
export enum Users_Skupiny_Update_Column {
  /** column name */
  UsColor = 'us_color',
  /** column name */
  UsId = 'us_id',
  /** column name */
  UsPlatbaCtvrtrok = 'us_platba_ctvrtrok',
  /** column name */
  UsPlatbaMesic = 'us_platba_mesic',
  /** column name */
  UsPlatbaPulrok = 'us_platba_pulrok',
  /** column name */
  UsPopis = 'us_popis'
}

/** aggregate var_pop on columns */
export type Users_Skupiny_Var_Pop_Fields = {
  __typename?: 'users_skupiny_var_pop_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Users_Skupiny_Var_Samp_Fields = {
  __typename?: 'users_skupiny_var_samp_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Users_Skupiny_Variance_Fields = {
  __typename?: 'users_skupiny_variance_fields';
  us_id?: Maybe<Scalars['Float']>;
  us_platba_ctvrtrok?: Maybe<Scalars['Float']>;
  us_platba_mesic?: Maybe<Scalars['Float']>;
  us_platba_pulrok?: Maybe<Scalars['Float']>;
};

/** aggregate stddev on columns */
export type Users_Stddev_Fields = {
  __typename?: 'users_stddev_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev() on columns of table "users" */
export type Users_Stddev_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_pop on columns */
export type Users_Stddev_Pop_Fields = {
  __typename?: 'users_stddev_pop_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_pop() on columns of table "users" */
export type Users_Stddev_Pop_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** aggregate stddev_samp on columns */
export type Users_Stddev_Samp_Fields = {
  __typename?: 'users_stddev_samp_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by stddev_samp() on columns of table "users" */
export type Users_Stddev_Samp_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** aggregate sum on columns */
export type Users_Sum_Fields = {
  __typename?: 'users_sum_fields';
  u_group?: Maybe<Scalars['bigint']>;
  u_id?: Maybe<Scalars['bigint']>;
  u_level?: Maybe<Scalars['smallint']>;
  u_skupina?: Maybe<Scalars['bigint']>;
};

/** order by sum() on columns of table "users" */
export type Users_Sum_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** update columns of table "users" */
export enum Users_Update_Column {
  /** column name */
  UBan = 'u_ban',
  /** column name */
  UCity = 'u_city',
  /** column name */
  UConfirmed = 'u_confirmed',
  /** column name */
  UConscriptionNumber = 'u_conscription_number',
  /** column name */
  UCreatedAt = 'u_created_at',
  /** column name */
  UDancer = 'u_dancer',
  /** column name */
  UDistrict = 'u_district',
  /** column name */
  UEmail = 'u_email',
  /** column name */
  UGdprSignedAt = 'u_gdpr_signed_at',
  /** column name */
  UGroup = 'u_group',
  /** column name */
  UId = 'u_id',
  /** column name */
  UJmeno = 'u_jmeno',
  /** column name */
  ULevel = 'u_level',
  /** column name */
  ULock = 'u_lock',
  /** column name */
  ULogin = 'u_login',
  /** column name */
  UMemberSince = 'u_member_since',
  /** column name */
  UMemberUntil = 'u_member_until',
  /** column name */
  UNarozeni = 'u_narozeni',
  /** column name */
  UNationality = 'u_nationality',
  /** column name */
  UOrientationNumber = 'u_orientation_number',
  /** column name */
  UPass = 'u_pass',
  /** column name */
  UPohlavi = 'u_pohlavi',
  /** column name */
  UPostalCode = 'u_postal_code',
  /** column name */
  UPoznamky = 'u_poznamky',
  /** column name */
  UPrijmeni = 'u_prijmeni',
  /** column name */
  URodneCislo = 'u_rodne_cislo',
  /** column name */
  USkupina = 'u_skupina',
  /** column name */
  UStreet = 'u_street',
  /** column name */
  USystem = 'u_system',
  /** column name */
  UTeacher = 'u_teacher',
  /** column name */
  UTelefon = 'u_telefon',
  /** column name */
  UTimestamp = 'u_timestamp'
}

/** aggregate var_pop on columns */
export type Users_Var_Pop_Fields = {
  __typename?: 'users_var_pop_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_pop() on columns of table "users" */
export type Users_Var_Pop_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** aggregate var_samp on columns */
export type Users_Var_Samp_Fields = {
  __typename?: 'users_var_samp_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by var_samp() on columns of table "users" */
export type Users_Var_Samp_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** aggregate variance on columns */
export type Users_Variance_Fields = {
  __typename?: 'users_variance_fields';
  u_group?: Maybe<Scalars['Float']>;
  u_id?: Maybe<Scalars['Float']>;
  u_level?: Maybe<Scalars['Float']>;
  u_skupina?: Maybe<Scalars['Float']>;
};

/** order by variance() on columns of table "users" */
export type Users_Variance_Order_By = {
  u_group?: InputMaybe<Order_By>;
  u_id?: InputMaybe<Order_By>;
  u_level?: InputMaybe<Order_By>;
  u_skupina?: InputMaybe<Order_By>;
};

/** columns and relationships of "video" */
export type Video = {
  __typename?: 'video';
  v_author: Scalars['String'];
  v_created_at: Scalars['timestamptz'];
  v_description: Scalars['String'];
  v_id: Scalars['bigint'];
  v_playlist?: Maybe<Scalars['String']>;
  v_title: Scalars['String'];
  v_updated_at: Scalars['timestamptz'];
  v_uri: Scalars['String'];
};

/** aggregated selection of "video" */
export type Video_Aggregate = {
  __typename?: 'video_aggregate';
  aggregate?: Maybe<Video_Aggregate_Fields>;
  nodes: Array<Video>;
};

/** aggregate fields of "video" */
export type Video_Aggregate_Fields = {
  __typename?: 'video_aggregate_fields';
  avg?: Maybe<Video_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Video_Max_Fields>;
  min?: Maybe<Video_Min_Fields>;
  stddev?: Maybe<Video_Stddev_Fields>;
  stddev_pop?: Maybe<Video_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Video_Stddev_Samp_Fields>;
  sum?: Maybe<Video_Sum_Fields>;
  var_pop?: Maybe<Video_Var_Pop_Fields>;
  var_samp?: Maybe<Video_Var_Samp_Fields>;
  variance?: Maybe<Video_Variance_Fields>;
};


/** aggregate fields of "video" */
export type Video_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Video_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Video_Avg_Fields = {
  __typename?: 'video_avg_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "video". All fields are combined with a logical 'AND'. */
export type Video_Bool_Exp = {
  _and?: InputMaybe<Array<Video_Bool_Exp>>;
  _not?: InputMaybe<Video_Bool_Exp>;
  _or?: InputMaybe<Array<Video_Bool_Exp>>;
  v_author?: InputMaybe<String_Comparison_Exp>;
  v_created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  v_description?: InputMaybe<String_Comparison_Exp>;
  v_id?: InputMaybe<Bigint_Comparison_Exp>;
  v_playlist?: InputMaybe<String_Comparison_Exp>;
  v_title?: InputMaybe<String_Comparison_Exp>;
  v_updated_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  v_uri?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "video" */
export enum Video_Constraint {
  /** unique or primary key constraint */
  Idx_24821Primary = 'idx_24821_primary'
}

/** input type for incrementing numeric columns in table "video" */
export type Video_Inc_Input = {
  v_id?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "video" */
export type Video_Insert_Input = {
  v_author?: InputMaybe<Scalars['String']>;
  v_created_at?: InputMaybe<Scalars['timestamptz']>;
  v_description?: InputMaybe<Scalars['String']>;
  v_id?: InputMaybe<Scalars['bigint']>;
  v_playlist?: InputMaybe<Scalars['String']>;
  v_title?: InputMaybe<Scalars['String']>;
  v_updated_at?: InputMaybe<Scalars['timestamptz']>;
  v_uri?: InputMaybe<Scalars['String']>;
};

/** columns and relationships of "video_list" */
export type Video_List = {
  __typename?: 'video_list';
  vl_count: Scalars['bigint'];
  vl_created_at: Scalars['timestamptz'];
  vl_description: Scalars['String'];
  vl_id: Scalars['bigint'];
  vl_last_checked?: Maybe<Scalars['timestamptz']>;
  vl_title: Scalars['String'];
  vl_url: Scalars['String'];
};

/** aggregated selection of "video_list" */
export type Video_List_Aggregate = {
  __typename?: 'video_list_aggregate';
  aggregate?: Maybe<Video_List_Aggregate_Fields>;
  nodes: Array<Video_List>;
};

/** aggregate fields of "video_list" */
export type Video_List_Aggregate_Fields = {
  __typename?: 'video_list_aggregate_fields';
  avg?: Maybe<Video_List_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Video_List_Max_Fields>;
  min?: Maybe<Video_List_Min_Fields>;
  stddev?: Maybe<Video_List_Stddev_Fields>;
  stddev_pop?: Maybe<Video_List_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Video_List_Stddev_Samp_Fields>;
  sum?: Maybe<Video_List_Sum_Fields>;
  var_pop?: Maybe<Video_List_Var_Pop_Fields>;
  var_samp?: Maybe<Video_List_Var_Samp_Fields>;
  variance?: Maybe<Video_List_Variance_Fields>;
};


/** aggregate fields of "video_list" */
export type Video_List_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Video_List_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Video_List_Avg_Fields = {
  __typename?: 'video_list_avg_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "video_list". All fields are combined with a logical 'AND'. */
export type Video_List_Bool_Exp = {
  _and?: InputMaybe<Array<Video_List_Bool_Exp>>;
  _not?: InputMaybe<Video_List_Bool_Exp>;
  _or?: InputMaybe<Array<Video_List_Bool_Exp>>;
  vl_count?: InputMaybe<Bigint_Comparison_Exp>;
  vl_created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  vl_description?: InputMaybe<String_Comparison_Exp>;
  vl_id?: InputMaybe<Bigint_Comparison_Exp>;
  vl_last_checked?: InputMaybe<Timestamptz_Comparison_Exp>;
  vl_title?: InputMaybe<String_Comparison_Exp>;
  vl_url?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "video_list" */
export enum Video_List_Constraint {
  /** unique or primary key constraint */
  Idx_24831Primary = 'idx_24831_primary'
}

/** input type for incrementing numeric columns in table "video_list" */
export type Video_List_Inc_Input = {
  vl_count?: InputMaybe<Scalars['bigint']>;
  vl_id?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "video_list" */
export type Video_List_Insert_Input = {
  vl_count?: InputMaybe<Scalars['bigint']>;
  vl_created_at?: InputMaybe<Scalars['timestamptz']>;
  vl_description?: InputMaybe<Scalars['String']>;
  vl_id?: InputMaybe<Scalars['bigint']>;
  vl_last_checked?: InputMaybe<Scalars['timestamptz']>;
  vl_title?: InputMaybe<Scalars['String']>;
  vl_url?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Video_List_Max_Fields = {
  __typename?: 'video_list_max_fields';
  vl_count?: Maybe<Scalars['bigint']>;
  vl_created_at?: Maybe<Scalars['timestamptz']>;
  vl_description?: Maybe<Scalars['String']>;
  vl_id?: Maybe<Scalars['bigint']>;
  vl_last_checked?: Maybe<Scalars['timestamptz']>;
  vl_title?: Maybe<Scalars['String']>;
  vl_url?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Video_List_Min_Fields = {
  __typename?: 'video_list_min_fields';
  vl_count?: Maybe<Scalars['bigint']>;
  vl_created_at?: Maybe<Scalars['timestamptz']>;
  vl_description?: Maybe<Scalars['String']>;
  vl_id?: Maybe<Scalars['bigint']>;
  vl_last_checked?: Maybe<Scalars['timestamptz']>;
  vl_title?: Maybe<Scalars['String']>;
  vl_url?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "video_list" */
export type Video_List_Mutation_Response = {
  __typename?: 'video_list_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Video_List>;
};

/** on conflict condition type for table "video_list" */
export type Video_List_On_Conflict = {
  constraint: Video_List_Constraint;
  update_columns?: Array<Video_List_Update_Column>;
  where?: InputMaybe<Video_List_Bool_Exp>;
};

/** Ordering options when selecting data from "video_list". */
export type Video_List_Order_By = {
  vl_count?: InputMaybe<Order_By>;
  vl_created_at?: InputMaybe<Order_By>;
  vl_description?: InputMaybe<Order_By>;
  vl_id?: InputMaybe<Order_By>;
  vl_last_checked?: InputMaybe<Order_By>;
  vl_title?: InputMaybe<Order_By>;
  vl_url?: InputMaybe<Order_By>;
};

/** primary key columns input for table: video_list */
export type Video_List_Pk_Columns_Input = {
  vl_id: Scalars['bigint'];
};

/** select columns of table "video_list" */
export enum Video_List_Select_Column {
  /** column name */
  VlCount = 'vl_count',
  /** column name */
  VlCreatedAt = 'vl_created_at',
  /** column name */
  VlDescription = 'vl_description',
  /** column name */
  VlId = 'vl_id',
  /** column name */
  VlLastChecked = 'vl_last_checked',
  /** column name */
  VlTitle = 'vl_title',
  /** column name */
  VlUrl = 'vl_url'
}

/** input type for updating data in table "video_list" */
export type Video_List_Set_Input = {
  vl_count?: InputMaybe<Scalars['bigint']>;
  vl_created_at?: InputMaybe<Scalars['timestamptz']>;
  vl_description?: InputMaybe<Scalars['String']>;
  vl_id?: InputMaybe<Scalars['bigint']>;
  vl_last_checked?: InputMaybe<Scalars['timestamptz']>;
  vl_title?: InputMaybe<Scalars['String']>;
  vl_url?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Video_List_Stddev_Fields = {
  __typename?: 'video_list_stddev_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Video_List_Stddev_Pop_Fields = {
  __typename?: 'video_list_stddev_pop_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Video_List_Stddev_Samp_Fields = {
  __typename?: 'video_list_stddev_samp_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Video_List_Sum_Fields = {
  __typename?: 'video_list_sum_fields';
  vl_count?: Maybe<Scalars['bigint']>;
  vl_id?: Maybe<Scalars['bigint']>;
};

/** update columns of table "video_list" */
export enum Video_List_Update_Column {
  /** column name */
  VlCount = 'vl_count',
  /** column name */
  VlCreatedAt = 'vl_created_at',
  /** column name */
  VlDescription = 'vl_description',
  /** column name */
  VlId = 'vl_id',
  /** column name */
  VlLastChecked = 'vl_last_checked',
  /** column name */
  VlTitle = 'vl_title',
  /** column name */
  VlUrl = 'vl_url'
}

/** aggregate var_pop on columns */
export type Video_List_Var_Pop_Fields = {
  __typename?: 'video_list_var_pop_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Video_List_Var_Samp_Fields = {
  __typename?: 'video_list_var_samp_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Video_List_Variance_Fields = {
  __typename?: 'video_list_variance_fields';
  vl_count?: Maybe<Scalars['Float']>;
  vl_id?: Maybe<Scalars['Float']>;
};

/** aggregate max on columns */
export type Video_Max_Fields = {
  __typename?: 'video_max_fields';
  v_author?: Maybe<Scalars['String']>;
  v_created_at?: Maybe<Scalars['timestamptz']>;
  v_description?: Maybe<Scalars['String']>;
  v_id?: Maybe<Scalars['bigint']>;
  v_playlist?: Maybe<Scalars['String']>;
  v_title?: Maybe<Scalars['String']>;
  v_updated_at?: Maybe<Scalars['timestamptz']>;
  v_uri?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Video_Min_Fields = {
  __typename?: 'video_min_fields';
  v_author?: Maybe<Scalars['String']>;
  v_created_at?: Maybe<Scalars['timestamptz']>;
  v_description?: Maybe<Scalars['String']>;
  v_id?: Maybe<Scalars['bigint']>;
  v_playlist?: Maybe<Scalars['String']>;
  v_title?: Maybe<Scalars['String']>;
  v_updated_at?: Maybe<Scalars['timestamptz']>;
  v_uri?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "video" */
export type Video_Mutation_Response = {
  __typename?: 'video_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Video>;
};

/** on conflict condition type for table "video" */
export type Video_On_Conflict = {
  constraint: Video_Constraint;
  update_columns?: Array<Video_Update_Column>;
  where?: InputMaybe<Video_Bool_Exp>;
};

/** Ordering options when selecting data from "video". */
export type Video_Order_By = {
  v_author?: InputMaybe<Order_By>;
  v_created_at?: InputMaybe<Order_By>;
  v_description?: InputMaybe<Order_By>;
  v_id?: InputMaybe<Order_By>;
  v_playlist?: InputMaybe<Order_By>;
  v_title?: InputMaybe<Order_By>;
  v_updated_at?: InputMaybe<Order_By>;
  v_uri?: InputMaybe<Order_By>;
};

/** primary key columns input for table: video */
export type Video_Pk_Columns_Input = {
  v_id: Scalars['bigint'];
};

/** select columns of table "video" */
export enum Video_Select_Column {
  /** column name */
  VAuthor = 'v_author',
  /** column name */
  VCreatedAt = 'v_created_at',
  /** column name */
  VDescription = 'v_description',
  /** column name */
  VId = 'v_id',
  /** column name */
  VPlaylist = 'v_playlist',
  /** column name */
  VTitle = 'v_title',
  /** column name */
  VUpdatedAt = 'v_updated_at',
  /** column name */
  VUri = 'v_uri'
}

/** input type for updating data in table "video" */
export type Video_Set_Input = {
  v_author?: InputMaybe<Scalars['String']>;
  v_created_at?: InputMaybe<Scalars['timestamptz']>;
  v_description?: InputMaybe<Scalars['String']>;
  v_id?: InputMaybe<Scalars['bigint']>;
  v_playlist?: InputMaybe<Scalars['String']>;
  v_title?: InputMaybe<Scalars['String']>;
  v_updated_at?: InputMaybe<Scalars['timestamptz']>;
  v_uri?: InputMaybe<Scalars['String']>;
};

/** columns and relationships of "video_source" */
export type Video_Source = {
  __typename?: 'video_source';
  vs_created_at: Scalars['timestamptz'];
  vs_description?: Maybe<Scalars['String']>;
  vs_id: Scalars['bigint'];
  vs_last_checked?: Maybe<Scalars['timestamptz']>;
  vs_title?: Maybe<Scalars['String']>;
  vs_url: Scalars['String'];
};

/** aggregated selection of "video_source" */
export type Video_Source_Aggregate = {
  __typename?: 'video_source_aggregate';
  aggregate?: Maybe<Video_Source_Aggregate_Fields>;
  nodes: Array<Video_Source>;
};

/** aggregate fields of "video_source" */
export type Video_Source_Aggregate_Fields = {
  __typename?: 'video_source_aggregate_fields';
  avg?: Maybe<Video_Source_Avg_Fields>;
  count: Scalars['Int'];
  max?: Maybe<Video_Source_Max_Fields>;
  min?: Maybe<Video_Source_Min_Fields>;
  stddev?: Maybe<Video_Source_Stddev_Fields>;
  stddev_pop?: Maybe<Video_Source_Stddev_Pop_Fields>;
  stddev_samp?: Maybe<Video_Source_Stddev_Samp_Fields>;
  sum?: Maybe<Video_Source_Sum_Fields>;
  var_pop?: Maybe<Video_Source_Var_Pop_Fields>;
  var_samp?: Maybe<Video_Source_Var_Samp_Fields>;
  variance?: Maybe<Video_Source_Variance_Fields>;
};


/** aggregate fields of "video_source" */
export type Video_Source_Aggregate_FieldsCountArgs = {
  columns?: InputMaybe<Array<Video_Source_Select_Column>>;
  distinct?: InputMaybe<Scalars['Boolean']>;
};

/** aggregate avg on columns */
export type Video_Source_Avg_Fields = {
  __typename?: 'video_source_avg_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** Boolean expression to filter rows from the table "video_source". All fields are combined with a logical 'AND'. */
export type Video_Source_Bool_Exp = {
  _and?: InputMaybe<Array<Video_Source_Bool_Exp>>;
  _not?: InputMaybe<Video_Source_Bool_Exp>;
  _or?: InputMaybe<Array<Video_Source_Bool_Exp>>;
  vs_created_at?: InputMaybe<Timestamptz_Comparison_Exp>;
  vs_description?: InputMaybe<String_Comparison_Exp>;
  vs_id?: InputMaybe<Bigint_Comparison_Exp>;
  vs_last_checked?: InputMaybe<Timestamptz_Comparison_Exp>;
  vs_title?: InputMaybe<String_Comparison_Exp>;
  vs_url?: InputMaybe<String_Comparison_Exp>;
};

/** unique or primary key constraints on table "video_source" */
export enum Video_Source_Constraint {
  /** unique or primary key constraint */
  Idx_24841Primary = 'idx_24841_primary'
}

/** input type for incrementing numeric columns in table "video_source" */
export type Video_Source_Inc_Input = {
  vs_id?: InputMaybe<Scalars['bigint']>;
};

/** input type for inserting data into table "video_source" */
export type Video_Source_Insert_Input = {
  vs_created_at?: InputMaybe<Scalars['timestamptz']>;
  vs_description?: InputMaybe<Scalars['String']>;
  vs_id?: InputMaybe<Scalars['bigint']>;
  vs_last_checked?: InputMaybe<Scalars['timestamptz']>;
  vs_title?: InputMaybe<Scalars['String']>;
  vs_url?: InputMaybe<Scalars['String']>;
};

/** aggregate max on columns */
export type Video_Source_Max_Fields = {
  __typename?: 'video_source_max_fields';
  vs_created_at?: Maybe<Scalars['timestamptz']>;
  vs_description?: Maybe<Scalars['String']>;
  vs_id?: Maybe<Scalars['bigint']>;
  vs_last_checked?: Maybe<Scalars['timestamptz']>;
  vs_title?: Maybe<Scalars['String']>;
  vs_url?: Maybe<Scalars['String']>;
};

/** aggregate min on columns */
export type Video_Source_Min_Fields = {
  __typename?: 'video_source_min_fields';
  vs_created_at?: Maybe<Scalars['timestamptz']>;
  vs_description?: Maybe<Scalars['String']>;
  vs_id?: Maybe<Scalars['bigint']>;
  vs_last_checked?: Maybe<Scalars['timestamptz']>;
  vs_title?: Maybe<Scalars['String']>;
  vs_url?: Maybe<Scalars['String']>;
};

/** response of any mutation on the table "video_source" */
export type Video_Source_Mutation_Response = {
  __typename?: 'video_source_mutation_response';
  /** number of rows affected by the mutation */
  affected_rows: Scalars['Int'];
  /** data from the rows affected by the mutation */
  returning: Array<Video_Source>;
};

/** on conflict condition type for table "video_source" */
export type Video_Source_On_Conflict = {
  constraint: Video_Source_Constraint;
  update_columns?: Array<Video_Source_Update_Column>;
  where?: InputMaybe<Video_Source_Bool_Exp>;
};

/** Ordering options when selecting data from "video_source". */
export type Video_Source_Order_By = {
  vs_created_at?: InputMaybe<Order_By>;
  vs_description?: InputMaybe<Order_By>;
  vs_id?: InputMaybe<Order_By>;
  vs_last_checked?: InputMaybe<Order_By>;
  vs_title?: InputMaybe<Order_By>;
  vs_url?: InputMaybe<Order_By>;
};

/** primary key columns input for table: video_source */
export type Video_Source_Pk_Columns_Input = {
  vs_id: Scalars['bigint'];
};

/** select columns of table "video_source" */
export enum Video_Source_Select_Column {
  /** column name */
  VsCreatedAt = 'vs_created_at',
  /** column name */
  VsDescription = 'vs_description',
  /** column name */
  VsId = 'vs_id',
  /** column name */
  VsLastChecked = 'vs_last_checked',
  /** column name */
  VsTitle = 'vs_title',
  /** column name */
  VsUrl = 'vs_url'
}

/** input type for updating data in table "video_source" */
export type Video_Source_Set_Input = {
  vs_created_at?: InputMaybe<Scalars['timestamptz']>;
  vs_description?: InputMaybe<Scalars['String']>;
  vs_id?: InputMaybe<Scalars['bigint']>;
  vs_last_checked?: InputMaybe<Scalars['timestamptz']>;
  vs_title?: InputMaybe<Scalars['String']>;
  vs_url?: InputMaybe<Scalars['String']>;
};

/** aggregate stddev on columns */
export type Video_Source_Stddev_Fields = {
  __typename?: 'video_source_stddev_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Video_Source_Stddev_Pop_Fields = {
  __typename?: 'video_source_stddev_pop_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Video_Source_Stddev_Samp_Fields = {
  __typename?: 'video_source_stddev_samp_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Video_Source_Sum_Fields = {
  __typename?: 'video_source_sum_fields';
  vs_id?: Maybe<Scalars['bigint']>;
};

/** update columns of table "video_source" */
export enum Video_Source_Update_Column {
  /** column name */
  VsCreatedAt = 'vs_created_at',
  /** column name */
  VsDescription = 'vs_description',
  /** column name */
  VsId = 'vs_id',
  /** column name */
  VsLastChecked = 'vs_last_checked',
  /** column name */
  VsTitle = 'vs_title',
  /** column name */
  VsUrl = 'vs_url'
}

/** aggregate var_pop on columns */
export type Video_Source_Var_Pop_Fields = {
  __typename?: 'video_source_var_pop_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Video_Source_Var_Samp_Fields = {
  __typename?: 'video_source_var_samp_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Video_Source_Variance_Fields = {
  __typename?: 'video_source_variance_fields';
  vs_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev on columns */
export type Video_Stddev_Fields = {
  __typename?: 'video_stddev_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_pop on columns */
export type Video_Stddev_Pop_Fields = {
  __typename?: 'video_stddev_pop_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** aggregate stddev_samp on columns */
export type Video_Stddev_Samp_Fields = {
  __typename?: 'video_stddev_samp_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** aggregate sum on columns */
export type Video_Sum_Fields = {
  __typename?: 'video_sum_fields';
  v_id?: Maybe<Scalars['bigint']>;
};

/** update columns of table "video" */
export enum Video_Update_Column {
  /** column name */
  VAuthor = 'v_author',
  /** column name */
  VCreatedAt = 'v_created_at',
  /** column name */
  VDescription = 'v_description',
  /** column name */
  VId = 'v_id',
  /** column name */
  VPlaylist = 'v_playlist',
  /** column name */
  VTitle = 'v_title',
  /** column name */
  VUpdatedAt = 'v_updated_at',
  /** column name */
  VUri = 'v_uri'
}

/** aggregate var_pop on columns */
export type Video_Var_Pop_Fields = {
  __typename?: 'video_var_pop_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** aggregate var_samp on columns */
export type Video_Var_Samp_Fields = {
  __typename?: 'video_var_samp_fields';
  v_id?: Maybe<Scalars['Float']>;
};

/** aggregate variance on columns */
export type Video_Variance_Fields = {
  __typename?: 'video_variance_fields';
  v_id?: Maybe<Scalars['Float']>;
};

export type UpozorneniListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type UpozorneniListQuery = { __typename?: 'query_root', upozorneni: Array<{ __typename?: 'upozorneni', up_id: any, up_kdo: any, up_lock: boolean, up_nadpis: string, up_text: string, up_timestamp?: any | null | undefined, up_timestamp_add: any, user: { __typename?: 'users', u_id: any, u_jmeno: string, u_prijmeni: string }, upozorneni_skupinies: Array<{ __typename?: 'upozorneni_skupiny', skupiny: { __typename?: 'skupiny', s_name: string, s_description: string, s_color_text: string, s_color_rgb: string } }> }>, aggregate: { __typename?: 'upozorneni_aggregate', aggregate?: { __typename?: 'upozorneni_aggregate_fields', count: number } | null | undefined } };

export type ArticlesAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ArticlesAdminListQuery = { __typename?: 'query_root', aktuality: Array<{ __typename?: 'aktuality_admin', at_foto?: any | null | undefined, at_foto_main?: any | null | undefined, at_id?: any | null | undefined, at_jmeno?: string | null | undefined, at_kat?: string | null | undefined, at_kdo?: any | null | undefined, at_preview?: string | null | undefined, at_text?: string | null | undefined, at_timestamp_add?: any | null | undefined, at_timestamp?: any | null | undefined }>, aggregate: { __typename?: 'aktuality_admin_aggregate', aggregate?: { __typename?: 'aktuality_admin_aggregate_fields', count: number } | null | undefined } };

export type AkceListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type AkceListQuery = { __typename?: 'query_root', akce: Array<{ __typename?: 'akce', a_do: any, a_id: any, a_info: string, a_dokumenty: string, a_jmeno: string, a_kapacita: any, a_kde: string, a_lock: boolean, a_od: any, a_timestamp?: any | null | undefined, a_visible: boolean, akce_items: Array<{ __typename?: 'akce_item', ai_id: any, user: { __typename?: 'users', u_jmeno: string, u_prijmeni: string, u_id: any } }>, aggregate: { __typename?: 'akce_item_aggregate', aggregate?: { __typename?: 'akce_item_aggregate_fields', count: number } | null | undefined } }>, aggregate: { __typename?: 'akce_aggregate', aggregate?: { __typename?: 'akce_aggregate_fields', count: number } | null | undefined } };

export type SetAkceVisibleMutationVariables = Exact<{
  id: Scalars['bigint'];
  visible: Scalars['Boolean'];
}>;


export type SetAkceVisibleMutation = { __typename?: 'mutation_root', update_akce_by_pk?: { __typename?: 'akce', a_id: any } | null | undefined };

export type GalleryDirListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type GalleryDirListQuery = { __typename?: 'query_root', galerie_dir: Array<{ __typename?: 'galerie_dir', gd_hidden: boolean, gd_id: any, gd_id_rodic: any, gd_level: any, gd_name: string, gd_path: string }>, aggregate: { __typename?: 'galerie_dir_aggregate', aggregate?: { __typename?: 'galerie_dir_aggregate_fields', count: number } | null | undefined } };

export type SetGalerieDirVisibleMutationVariables = Exact<{
  id: Scalars['bigint'];
  visible: Scalars['Boolean'];
}>;


export type SetGalerieDirVisibleMutation = { __typename?: 'mutation_root', update_galerie_dir_by_pk?: { __typename?: 'galerie_dir', gd_id: any } | null | undefined };

export type ReservationAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ReservationAdminListQuery = { __typename?: 'query_root', nabidka: Array<{ __typename?: 'nabidka_admin', n_visible?: boolean | null | undefined, n_trener?: any | null | undefined, n_timestamp?: any | null | undefined, n_pocet_hod?: any | null | undefined, n_od?: any | null | undefined, n_max_pocet_hod?: any | null | undefined, n_lock?: boolean | null | undefined, n_id?: any | null | undefined, n_do?: any | null | undefined, user?: { __typename?: 'users', u_jmeno: string, u_prijmeni: string, u_id: any } | null | undefined, nabidka_items: Array<{ __typename?: 'nabidka_item', ni_lock: boolean, ni_partner: any, ni_pocet_hod: any, pary: { __typename?: 'pary', user: { __typename?: 'users', u_id: any, u_jmeno: string, u_prijmeni: string } } }> }>, aggregate: { __typename?: 'nabidka_admin_aggregate', aggregate?: { __typename?: 'nabidka_admin_aggregate_fields', count: number } | null | undefined } };

export type SetNabidkaVisibleMutationVariables = Exact<{
  id: Scalars['bigint'];
  visible: Scalars['Boolean'];
}>;


export type SetNabidkaVisibleMutation = { __typename?: 'mutation_root', update_nabidka_admin?: { __typename?: 'nabidka_admin_mutation_response', affected_rows: number } | null | undefined };

export type NabidkaListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type NabidkaListQuery = { __typename?: 'query_root', nabidka: Array<{ __typename?: 'nabidka', n_visible: boolean, n_trener: any, n_timestamp?: any | null | undefined, n_pocet_hod: any, n_od: any, n_max_pocet_hod: any, n_lock: boolean, n_id: any, n_do: any, user: { __typename?: 'users', u_jmeno: string, u_prijmeni: string, u_id: any }, nabidka_items: Array<{ __typename?: 'nabidka_item', ni_lock: boolean, ni_partner: any, ni_pocet_hod: any, pary: { __typename?: 'pary', user: { __typename?: 'users', u_id: any, u_jmeno: string, u_prijmeni: string } } }> }> };

export type ScheduleAdminListQueryVariables = Exact<{
  offset?: InputMaybe<Scalars['Int']>;
  limit?: InputMaybe<Scalars['Int']>;
}>;


export type ScheduleAdminListQuery = { __typename?: 'query_root', rozpis: Array<{ __typename?: 'rozpis_admin', r_datum?: any | null | undefined, r_id?: any | null | undefined, r_kde?: string | null | undefined, r_lock?: boolean | null | undefined, r_timestamp?: any | null | undefined, r_trener?: any | null | undefined, r_visible?: boolean | null | undefined, user?: { __typename?: 'users', u_jmeno: string, u_prijmeni: string, u_id: any } | null | undefined, rozpis_items: Array<{ __typename?: 'rozpis_item', ri_od: any, ri_do: any, ri_id: any, ri_partner?: any | null | undefined }> }>, aggregate: { __typename?: 'rozpis_admin_aggregate', aggregate?: { __typename?: 'rozpis_admin_aggregate_fields', count: number } | null | undefined } };

export type SetRozpisVisibleMutationVariables = Exact<{
  id: Scalars['bigint'];
  visible: Scalars['Boolean'];
}>;


export type SetRozpisVisibleMutation = { __typename?: 'mutation_root', update_rozpis_admin?: { __typename?: 'rozpis_admin_mutation_response', affected_rows: number } | null | undefined };

export type GetMenuQueryVariables = Exact<{ [key: string]: never; }>;


export type GetMenuQuery = { __typename?: 'query_root', parameters_by_pk?: { __typename?: 'parameters', pa_value: string } | null | undefined };


export const UpozorneniListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"UpozorneniList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"upozorneni"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"up_timestamp_add"},"value":{"kind":"EnumValue","value":"desc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"up_id"}},{"kind":"Field","name":{"kind":"Name","value":"up_kdo"}},{"kind":"Field","name":{"kind":"Name","value":"up_lock"}},{"kind":"Field","name":{"kind":"Name","value":"up_nadpis"}},{"kind":"Field","name":{"kind":"Name","value":"up_text"}},{"kind":"Field","name":{"kind":"Name","value":"up_timestamp"}},{"kind":"Field","name":{"kind":"Name","value":"up_timestamp_add"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_id"}},{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}}]}},{"kind":"Field","name":{"kind":"Name","value":"upozorneni_skupinies"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"skupiny"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"s_name"}},{"kind":"Field","name":{"kind":"Name","value":"s_description"}},{"kind":"Field","name":{"kind":"Name","value":"s_color_text"}},{"kind":"Field","name":{"kind":"Name","value":"s_color_rgb"}}]}}]}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"upozorneni_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<UpozorneniListQuery, UpozorneniListQueryVariables>;
export const ArticlesAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ArticlesAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"aktuality"},"name":{"kind":"Name","value":"aktuality_admin"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"at_timestamp_add"},"value":{"kind":"EnumValue","value":"desc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"at_foto"}},{"kind":"Field","name":{"kind":"Name","value":"at_foto_main"}},{"kind":"Field","name":{"kind":"Name","value":"at_id"}},{"kind":"Field","name":{"kind":"Name","value":"at_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"at_kat"}},{"kind":"Field","name":{"kind":"Name","value":"at_kdo"}},{"kind":"Field","name":{"kind":"Name","value":"at_preview"}},{"kind":"Field","name":{"kind":"Name","value":"at_text"}},{"kind":"Field","name":{"kind":"Name","value":"at_timestamp_add"}},{"kind":"Field","name":{"kind":"Name","value":"at_timestamp"}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"aktuality_admin_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<ArticlesAdminListQuery, ArticlesAdminListQueryVariables>;
export const AkceListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"AkceList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"akce"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"a_od"},"value":{"kind":"EnumValue","value":"desc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"a_do"}},{"kind":"Field","name":{"kind":"Name","value":"a_id"}},{"kind":"Field","name":{"kind":"Name","value":"a_info"}},{"kind":"Field","name":{"kind":"Name","value":"a_dokumenty"}},{"kind":"Field","name":{"kind":"Name","value":"a_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"a_kapacita"}},{"kind":"Field","name":{"kind":"Name","value":"a_kde"}},{"kind":"Field","name":{"kind":"Name","value":"a_lock"}},{"kind":"Field","name":{"kind":"Name","value":"a_od"}},{"kind":"Field","name":{"kind":"Name","value":"a_timestamp"}},{"kind":"Field","name":{"kind":"Name","value":"a_visible"}},{"kind":"Field","name":{"kind":"Name","value":"akce_items"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"ai_id"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"u_id"}}]}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"akce_items_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"akce_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<AkceListQuery, AkceListQueryVariables>;
export const SetAkceVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setAkceVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"bigint"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"update_akce_by_pk"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"pk_columns"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"a_id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}},{"kind":"Argument","name":{"kind":"Name","value":"_set"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"a_visible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"a_id"}}]}}]}}]} as unknown as DocumentNode<SetAkceVisibleMutation, SetAkceVisibleMutationVariables>;
export const GalleryDirListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GalleryDirList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"galerie_dir"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"gd_name"},"value":{"kind":"EnumValue","value":"asc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gd_hidden"}},{"kind":"Field","name":{"kind":"Name","value":"gd_id"}},{"kind":"Field","name":{"kind":"Name","value":"gd_id_rodic"}},{"kind":"Field","name":{"kind":"Name","value":"gd_level"}},{"kind":"Field","name":{"kind":"Name","value":"gd_name"}},{"kind":"Field","name":{"kind":"Name","value":"gd_path"}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"galerie_dir_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<GalleryDirListQuery, GalleryDirListQueryVariables>;
export const SetGalerieDirVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setGalerieDirVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"bigint"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"update_galerie_dir_by_pk"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"pk_columns"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"gd_id"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}},{"kind":"Argument","name":{"kind":"Name","value":"_set"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"gd_hidden"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"gd_id"}}]}}]}}]} as unknown as DocumentNode<SetGalerieDirVisibleMutation, SetGalerieDirVisibleMutationVariables>;
export const ReservationAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ReservationAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"nabidka"},"name":{"kind":"Name","value":"nabidka_admin"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"n_od"},"value":{"kind":"EnumValue","value":"desc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"n_visible"}},{"kind":"Field","name":{"kind":"Name","value":"n_trener"}},{"kind":"Field","name":{"kind":"Name","value":"n_timestamp"}},{"kind":"Field","name":{"kind":"Name","value":"n_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"n_od"}},{"kind":"Field","name":{"kind":"Name","value":"n_max_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"n_lock"}},{"kind":"Field","name":{"kind":"Name","value":"n_id"}},{"kind":"Field","name":{"kind":"Name","value":"n_do"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"u_id"}}]}},{"kind":"Field","name":{"kind":"Name","value":"nabidka_items"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"ni_lock"}},{"kind":"Field","name":{"kind":"Name","value":"ni_partner"}},{"kind":"Field","name":{"kind":"Name","value":"ni_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"pary"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_id"}},{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}}]}}]}}]}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"nabidka_admin_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<ReservationAdminListQuery, ReservationAdminListQueryVariables>;
export const SetNabidkaVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setNabidkaVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"bigint"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"update_nabidka_admin"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"where"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"n_id"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"_eq"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}]}},{"kind":"Argument","name":{"kind":"Name","value":"_set"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"n_visible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"affected_rows"}}]}}]}}]} as unknown as DocumentNode<SetNabidkaVisibleMutation, SetNabidkaVisibleMutationVariables>;
export const NabidkaListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"NabidkaList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"nabidka"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"n_visible"}},{"kind":"Field","name":{"kind":"Name","value":"n_trener"}},{"kind":"Field","name":{"kind":"Name","value":"n_timestamp"}},{"kind":"Field","name":{"kind":"Name","value":"n_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"n_od"}},{"kind":"Field","name":{"kind":"Name","value":"n_max_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"n_lock"}},{"kind":"Field","name":{"kind":"Name","value":"n_id"}},{"kind":"Field","name":{"kind":"Name","value":"n_do"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"u_id"}}]}},{"kind":"Field","name":{"kind":"Name","value":"nabidka_items"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"ni_lock"}},{"kind":"Field","name":{"kind":"Name","value":"ni_partner"}},{"kind":"Field","name":{"kind":"Name","value":"ni_pocet_hod"}},{"kind":"Field","name":{"kind":"Name","value":"pary"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_id"}},{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}}]}}]}}]}}]}}]}}]} as unknown as DocumentNode<NabidkaListQuery, NabidkaListQueryVariables>;
export const ScheduleAdminListDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"ScheduleAdminList"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"offset"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"limit"}},"type":{"kind":"NamedType","name":{"kind":"Name","value":"Int"}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","alias":{"kind":"Name","value":"rozpis"},"name":{"kind":"Name","value":"rozpis_admin"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"limit"},"value":{"kind":"Variable","name":{"kind":"Name","value":"limit"}}},{"kind":"Argument","name":{"kind":"Name","value":"offset"},"value":{"kind":"Variable","name":{"kind":"Name","value":"offset"}}},{"kind":"Argument","name":{"kind":"Name","value":"order_by"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"r_datum"},"value":{"kind":"EnumValue","value":"desc"}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"r_datum"}},{"kind":"Field","name":{"kind":"Name","value":"r_id"}},{"kind":"Field","name":{"kind":"Name","value":"r_kde"}},{"kind":"Field","name":{"kind":"Name","value":"r_lock"}},{"kind":"Field","name":{"kind":"Name","value":"r_timestamp"}},{"kind":"Field","name":{"kind":"Name","value":"r_trener"}},{"kind":"Field","name":{"kind":"Name","value":"r_visible"}},{"kind":"Field","name":{"kind":"Name","value":"user"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"u_jmeno"}},{"kind":"Field","name":{"kind":"Name","value":"u_prijmeni"}},{"kind":"Field","name":{"kind":"Name","value":"u_id"}}]}},{"kind":"Field","name":{"kind":"Name","value":"rozpis_items"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"ri_od"}},{"kind":"Field","name":{"kind":"Name","value":"ri_do"}},{"kind":"Field","name":{"kind":"Name","value":"ri_id"}},{"kind":"Field","name":{"kind":"Name","value":"ri_partner"}}]}}]}},{"kind":"Field","alias":{"kind":"Name","value":"aggregate"},"name":{"kind":"Name","value":"rozpis_admin_aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"aggregate"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"count"}}]}}]}}]}}]} as unknown as DocumentNode<ScheduleAdminListQuery, ScheduleAdminListQueryVariables>;
export const SetRozpisVisibleDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"mutation","name":{"kind":"Name","value":"setRozpisVisible"},"variableDefinitions":[{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"id"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"bigint"}}}},{"kind":"VariableDefinition","variable":{"kind":"Variable","name":{"kind":"Name","value":"visible"}},"type":{"kind":"NonNullType","type":{"kind":"NamedType","name":{"kind":"Name","value":"Boolean"}}}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"update_rozpis_admin"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"where"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"r_id"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"_eq"},"value":{"kind":"Variable","name":{"kind":"Name","value":"id"}}}]}}]}},{"kind":"Argument","name":{"kind":"Name","value":"_set"},"value":{"kind":"ObjectValue","fields":[{"kind":"ObjectField","name":{"kind":"Name","value":"r_visible"},"value":{"kind":"Variable","name":{"kind":"Name","value":"visible"}}}]}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"affected_rows"}}]}}]}}]} as unknown as DocumentNode<SetRozpisVisibleMutation, SetRozpisVisibleMutationVariables>;
export const GetMenuDocument = {"kind":"Document","definitions":[{"kind":"OperationDefinition","operation":"query","name":{"kind":"Name","value":"GetMenu"},"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"parameters_by_pk"},"arguments":[{"kind":"Argument","name":{"kind":"Name","value":"pa_name"},"value":{"kind":"StringValue","value":"menu","block":false}}],"selectionSet":{"kind":"SelectionSet","selections":[{"kind":"Field","name":{"kind":"Name","value":"pa_value"}}]}}]}}]} as unknown as DocumentNode<GetMenuQuery, GetMenuQueryVariables>;