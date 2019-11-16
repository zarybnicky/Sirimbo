{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Schema
  ( migrateAll
  , EntityField(..)
  , Unique(..)

  , EventId
  , Event(..)
  , EventItemId
  , EventItem(..)
  , ArticleId
  , Article(..)
  , DocumentId
  , Document(..)
  , PhotoDirectoryId
  , PhotoDirectory(..)
  , PhotoId
  , Photo(..)
  , ReservationId
  , Reservation(..)
  , ReservationItemId
  , ReservationItem(..)
  , UpdateLogId
  , UpdateLog(..)
  , ParameterId
  , Parameter(..)
  , CoupleId
  , Couple(..)
  , CoupleOfferId
  , CoupleOffer(..)
  , PermissionId
  , Permission(..)
  , PaymentCategoryId
  , PaymentCategory(..)
  , PaymentCategoryGroupId
  , PaymentCategoryGroup(..)
  , PaymentGroupId
  , PaymentGroup(..)
  , PaymentGroupUserGroupId
  , PaymentGroupUserGroup(..)
  , PaymentItemId
  , PaymentItem(..)
  , PaymentRawId
  , PaymentRaw(..)
  , ScheduleId
  , Schedule(..)
  , ScheduleItemId
  , ScheduleItem(..)
  , UserGroupId
  , UserGroup(..)
  , AnnouncementId
  , Announcement(..)
  , AnnouncementGroupId
  , AnnouncementGroup(..)
  , UserId
  , User(..)
  , VideoId
  , Video(..)
  , VideoListId
  , VideoList(..)
  , VideoSourceId
  , VideoSource(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, TimeOfDay, UTCTime)
import Database.Persist
import Database.Persist.TH
  (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Event sql=akce
  Id sql=a_id
  name Text sql=a_jmeno
  location Text sql=a_kde
  info Text sql=a_info
  from Day sql=a_od
  to Day sql=a_do
  capacity Int sql=a_kapacita default=0 sqltype=INT(11)
  documents Text sql=a_dokumenty
  updatedAt UTCTime sql=a_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  lock Text sql=a_lock sqltype=enum('0','1') default='0'
  visible Text sql=a_visible sqltype=enum('0','1') default='0'
  deriving Show

EventItem sql=akce_item
  Id sql=ai_id
  parent EventId sql=ai_id_rodic
  user UserId sql=ai_user
  birthYear Int sql=ai_rok_narozeni sqltype=SMALLINT(4)
  deriving Show

Article sql=aktuality
  Id sql=at_id
  createdBy UserId sql=at_kdo
  category Text sql=at_kat
  title Text sql=at_jmeno
  text Text sql=at_text
  preview Text sql=at_preview
  photo Int sql=at_foto MigrationOnly sqltype=INT(11)
  titlePhoto PhotoId sql=at_foto_main
  updatedAt UTCTime sql=at_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  createdAt UTCTime sql=at_timestamp_add sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

Document sql=dokumenty
  Id sql=d_id
  path Text sql=d_path
  name Text sql=d_name
  filename Text sql=d_filename
  category Text sql=d_kategorie sqltype=SMALLINT(3)
  createdBy UserId sql=d_kdo
  updatedAt UTCTime sql=d_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  UniqueDocument path sql=d_path
  deriving Show

PhotoDirectory sql=galerie_dir
  Id sql=gd_id
  parent PhotoDirectoryId sql=gd_id_rodic
  name Text sql=gd_name
  level Int sql=gd_level default=1 sqltype=SMALLINT(6)
  path Text sql=gd_path
  hidden Text sql=gd_hidden sqltype=enum('0','1') default='0'
  deriving Show

Photo sql=galerie_foto
  Id sql=gf_id
  parent PhotoDirectoryId sql=gf_id_rodic
  name Text sql=gf_name
  path Text sql=gf_path
  createdBy UserId sql=gf_kdo
  updatedAt UTCTime sql=gf_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

Reservation sql=nabidka
  Id sql=n_id
  trainer UserId sql=n_trener
  numberLessons Int sql=n_pocet_hod default=1 sqltype=SMALLINT(3)
  maximumPerPair Int sql=n_max_pocet_hod default=0 sqltype=INT(11)
  from Day sql=n_od
  to Day sql=n_do
  visible Text sql=n_visible sqltype=enum('0','1') default='0'
  lock Text sql=n_lock sqltype=enum('0','1') default='0'
  updatedAt UTCTime sql=n_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

ReservationItem sql=nabidka_item
  Id sql=ni_id
  parent ReservationId sql=ni_id_rodic
  partner UserId sql=ni_partner
  numberLessons Int sql=ni_pocet_hod default=1 sqltype=SMALLINT(3)
  lock Text sql=ni_lock sqltype=enum('0','1') default='0'
  UniqueReservationItem parent partner sql=ni_id_rodic
  deriving Show

UpdateLog sql=novinky
  Id sql=no_id
  createdBy UserId sql=no_id_user
  text Text sql=no_text
  createdAt UTCTime sql=no_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

Parameter sql=parameters
  Id Text sql=pa_name sqltype=VARCHAR(40)
  value Text sql=pa_value
  deriving Show

Couple sql=pary
  Id sql=p_id
  partnerLeader UserId sql=p_id_partner
  partnerFollower UserId sql=p_id_partnerka default=0
  standardClass Text sql=p_stt_trida default='Z' sqltype=enum('Z','H','D','C','B','A','M')
  standardPoints Int sql=p_stt_body default=0 "sqltype=SMALLINT(5) UNSIGNED"
  standardFinals Int sql=p_stt_finale default=0 "sqltype=TINYINT(1) UNSIGNED"
  latinClass Text sql=p_lat_trida default='Z' sqltype=enum('Z','H','D','C','B','A','M')
  latinPoints Int sql=p_lat_body default=0 "sqltype=SMALLINT(5) UNSIGNED"
  latinFinals Int sql=p_lat_finale default=0 "sqltype=TINYINT(1) UNSIGNED"
  score Int sql=p_hodnoceni default=0 "sqltype=MEDIUMINT(8) UNSIGNED"
  archived Text sql=p_archiv sqltype=enum('0','1') default='0'
  createdAt UTCTime sql=p_timestamp_add sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  archivedAt UTCTime sql=p_timestamp_archive sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

CoupleOffer sql=pary_navrh
  Id sql=pn_id
  createdBy UserId sql=pn_navrhl
  partnerLeader UserId sql=pn_partner
  partnerFollower UserId sql=pn_partnerka
  deriving Show

Permission sql=permissions
  Id sql=pe_id
  name Text sql=pe_name
  description Text sql=pe_description
  levelAkce Int sql=pe_akce sqltype=INT(2)
  levelAktuality Int sql=pe_aktuality sqltype=INT(2)
  levelAnkety Int sql=pe_ankety sqltype=INT(2)
  levelDokumenty Int sql=pe_dokumenty sqltype=INT(2)
  levelGalerie Int sql=pe_galerie sqltype=INT(2)
  levelInzerce Int sql=pe_inzerce sqltype=INT(2)
  levelKonzole Int sql=pe_konzole sqltype=INT(2)
  levelNabidka Int sql=pe_nabidka sqltype=INT(2)
  levelNastenka Int sql=pe_nastenka sqltype=INT(2)
  levelNovinky Int sql=pe_novinky sqltype=INT(2)
  levelPary Int sql=pe_pary sqltype=INT(2)
  levelPlatby Int sql=pe_platby sqltype=INT(2)
  levelPermissions Int sql=pe_permissions sqltype=INT(2)
  levelRozpis Int sql=pe_rozpis sqltype=INT(2)
  levelSkupiny Int sql=pe_skupiny sqltype=INT(2)
  levelUsers Int sql=pe_users sqltype=INT(2)
  levelMain Int sql=pe_main sqltype=INT(2)
  deriving Show

PaymentCategory sql=platby_category
  Id sql=pc_id
  name Text sql=pc_name
  symbol Int sql=pc_symbol sqltype=INT(11)
  amount Double sql=pc_amount sqltype=DECIMAL(10,2)
  dueDate Day sql=pc_date_due
  validFrom Day sql=pc_valid_from
  validTo Day sql=pc_valid_to
  useBase Text sql=pc_use_base sqltype=enum('0','1') default='0'
  usePrefix Text sql=pc_use_prefix sqltype=enum('0','1') default='0'
  archived Text sql=pc_archive sqltype=enum('0','1') default='0'
  visible Text sql=pc_visible sqltype=enum('0','1') default='1'
  deriving Show

PaymentCategoryGroup sql=platby_category_group
  Id sql=pcg_id
  group PaymentGroupId sql=pcg_id_group
  category PaymentCategoryId sql=pcg_id_category
  UniquePaymentCategoryGroup group category sql=pcg_id_group
  deriving Show

PaymentGroup sql=platby_group
  Id sql=pg_id
  type Text sql=pg_type sqltype=enum('0','1') default=0
  name Text sql=pg_name
  description Text sql=pg_description
  base Int sql=pg_base default=0 sqltype=INT(11)
  deriving Show

PaymentGroupUserGroup sql=platby_group_skupina
  Id sql=pgs_id
  userGroup UserGroupId sql=pgs_id_skupina
  paymentGroup PaymentGroupId sql=pgs_id_group
  UniquePaymentGroupUserGroup userGroup paymentGroup sql='pgs_id_skupina'
  deriving Show

PaymentItem sql=platby_item
  Id sql=pi_id
  user UserId sql=pi_id_user
  category PaymentCategoryId sql=pi_id_category
  raw PaymentRawId Maybe sql=pi_id_raw default=NULL
  amount Double sql=pi_amount sqltype=DECIMAL(10,2)
  date Day sql=pi_date
  prefix Int sql=pi_prefix default=2000 sqltype=INT(4)
  UniquePaymentItem raw sql=pi_id_raw !force
  deriving Show

PaymentRaw sql=platby_raw
  Id sql=pr_id
  raw ByteString sql=pr_raw
  hash Text sql=pr_hash sqltype=VARCHAR(32)
  sorted Text sql=pr_sorted sqltype=enum('0','1') default='0'
  discarded Text sql=pr_discarded sqltype=enum('0','1') default='0'

Schedule sql=rozpis
  Id sql=r_id
  trainer UserId sql=r_trener
  location Text sql=r_kde
  date Day sql=r_datum
  visible Text sql=r_visible sqltype=enum('0','1') default='0'
  lock Text sql=r_lock sqltype=enum('0','1') default='0'
  updatedAt UTCTime sql=r_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

ScheduleItem sql=rozpis_item
  Id sql=ri_id
  parent ScheduleId sql=ri_id_rodic
  partner UserId sql=ri_partner default=0
  from TimeOfDay sql=ri_od
  to TimeOfDay sql=ri_do
  lock Text sql=ri_lock sqltype=enum('0','1') default='0'
  deriving Show

UserGroup sql=skupiny
  Id sql=s_id
  name Text sql=s_name
  description Text sql=s_description
  colorRgb Text sql=s_color_rgb sqltype=VARCHAR(7)
  colorText Text sql=s_color_text sqltype=VARCHAR(20)
  deriving Show

Announcement sql=upozorneni
  Id sql=up_id
  createdBy UserId sql=up_kdo
  title Text sql=up_nadpis
  text Text sql=up_text
  colors Int sql=up_barvy default=0 sqltype=INT(11)
  lock Text sql=up_lock sqltype=enum('0','1') default='0'
  updatedAt UTCTime sql=up_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  createdAt UTCTime sql=up_timestamp_add sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

AnnouncementGroup sql=upozorneni_skupiny
  Id sql=ups_id
  parent AnnouncementId sql=ups_id_rodic
  userGroup UserGroupId sql=ups_id_skupina
  color Text sql=ups_color default='white' sqltype=VARCHAR(255)
  description Text sql=ups_popis
  deriving Show

Session
  Id Text sql=ss_id sqltype=VARCHAR(128)
  data Text sql=ss_data sqltype=JSON
  updatedAt UTCTime sql=ss_updated_at sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  lifetime Int sql=ss_lifetime

User sql=users
  Id sql=u_id
  login Text sql=u_login sqltype=CHAR(20)
  password Text sql=u_pass
  name Text sql=u_jmeno
  surname Text sql=u_prijmeni
  gender Text sql=u_pohlavi
  email Text sql=u_email
  phone Text sql=u_telefon sqltype=CHAR(17)
  birthDate Day sql=u_narozeni
  notes Text sql=u_poznamky default=''
  updatedAt UTCTime sql=u_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  level Int sql=u_level default=0 sqltype=TINYINT(3)
  paymentGroup PaymentGroupId sql=u_group
  userGroup UserGroupId sql=u_skupina default=1
  dancer Text sql=u_dancer sqltype=enum('0','1') default='0'
  ban Text sql=u_ban sqltype=enum('0','1') default='0'
  lock Text sql=u_lock sqltype=enum('0','1') default='0'
  confirmed Text sql=u_confirmed sqltype=enum('0','1') default='0'
  system Text sql=u_system sqltype=enum('0','1') default='0'
  street Text sql=u_street
  conscriptionNumber Text sql=u_conscription_number default=''
  orientationNumber Text sql=u_orientation_number default=''
  district Text sql=u_district default=''
  city Text sql=u_city
  postalCode Text sql=u_postal_code
  nationality Text sql=u_nationality
  memberSince UTCTime Maybe sql=u_member_since
  memberUntil UTCTime Maybe sql=u_member_until default=NULL
  createdAt UTCTime sql=u_created_at sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  teacher Text sql=u_teacher sqltype=enum('0','1') default='0'
  gdprSignedAt UTCTime Maybe sql=u_gdpr_signed_at
  deriving Show

Video sql=video
  Id sql=v_id
  uri Text sql=v_uri
  title Text sql=v_title
  author Text sql=v_author
  description Text sql=v_description
  playlistId Text Maybe sql=v_playlist
  createdAt UTCTime sql=v_created_at
  updatedAt UTCTime sql=v_updated_at sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

VideoList sql=video_list
  Id sql=vl_id
  url Text sql=vl_url
  title Text sql=vl_title
  description Text sql=vl_description
  itemCount Int sql=vl_count
  createdAt UTCTime sql=vl_created_at
  lastCheckedAt UTCTime Maybe sql=vl_last_checked sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

VideoSource sql=video_source
  Id sql=vs_id
  url Text sql=vs_url
  title Text Maybe sql=vs_title default=NULL
  description Text Maybe sql=vs_description default=NULL
  createdAt UTCTime sql=vs_created_at
  lastCheckedAt UTCTime Maybe sql=vs_last_checked sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show
|]
