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
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Event sql=akce
  Id sql=a_id
  name Text sql=a_jmeno sqltype=VARCHAR(255)
  location Text sql=a_kde sqltype=VARCHAR(255)
  info Text sql=a_info
  from Day sql=a_od
  to Day sql=a_do
  capacity Int sql=a_kapacita default=0 sqltype=INT(11)
  documents Text sql=a_dokumenty
  updatedAt UTCTime sql=a_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  lock Bool sql=a_lock default=0
  visible Bool sql=a_visible default=0
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
  title Text sql=at_jmeno sqltype=VARCHAR(255)
  text Text sql=at_text
  preview Text sql=at_preview sqltype=VARCHAR(200)
  photo Int sql=at_foto MigrationOnly sqltype=INT(11)
  titlePhoto PhotoId sql=at_foto_main
  updatedAt UTCTime sql=at_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  createdAt UTCTime sql=at_timestamp_add sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

Document sql=dokumenty
  Id sql=d_id
  path Text sql=d_path sqltype=VARCHAR(50)
  name Text sql=d_name sqltype=VARCHAR(80)
  filename Text sql=d_filename sqltype=VARCHAR(80)
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
  hidden Bool sql=gd_hidden default=0
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
  visible Bool sql=n_visible default=0
  lock Bool sql=n_lock default=0
  updatedAt UTCTime sql=n_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

ReservationItem sql=nabidka_item
  Id sql=ni_id
  parent ReservationId sql=ni_id_rodic
  partner UserId sql=ni_partner
  numberLessons Int sql=ni_pocet_hod default=1 sqltype=SMALLINT(3)
  lock Bool sql=ni_lock default=0
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
  standardPoints Int sql=p_stt_body default=0 sqltype=SMALLINT(5)
  standardFinals Int sql=p_stt_finale default=0 sqltype=TINYINT(1)
  latinClass Text sql=p_lat_trida default='Z' sqltype=enum('Z','H','D','C','B','A','M')
  latinPoints Int sql=p_lat_body default=0 sqltype=SMALLINT(5)
  latinFinals Int sql=p_lat_finale default=0 sqltype=TINYINT(1)
  score Int sql=p_hodnoceni default=0 sqltype=MEDIUMINT(8)
  archived Bool sql=p_archiv default=0
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
  name Text sql=pc_name sqltype=VARCHAR(50)
  symbol Int sql=pc_symbol sqltype=INT(11)
  amount Double sql=pc_amount sqltype=DECIMAL(10,2)
  dueDate Day sql=pc_date_due
  validFrom Day sql=pc_valid_from
  validTo Day sql=pc_valid_to
  useBase Bool sql=pc_use_base default=0
  usePrefix Bool sql=pc_use_prefix default=0
  archived Bool sql=pc_archive default=0
  visible Bool sql=pc_visible default=1
  deriving Show

PaymentCategoryGroup sql=platby_category_group
  Id sql=pcg_id
  group PaymentGroupId sql=pcg_id_group
  category PaymentCategoryId sql=pcg_id_category
  UniquePaymentCategoryGroup group category sql=pcg_id_group
  deriving Show

PaymentGroup sql=platby_group
  Id sql=pg_id
  type Int sql=pg_type default=0
  name Text sql=pg_name sqltype=VARCHAR(50)
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
  sorted Bool sql=pr_sorted default=0
  discarded Bool sql=pr_discarded default=0

Schedule sql=rozpis
  Id sql=r_id
  trainer UserId sql=r_trener
  location Text sql=r_kde sqltype=VARCHAR(100)
  date Day sql=r_datum
  visible Bool sql=r_visible default=0
  lock Bool sql=r_lock default=0
  updatedAt UTCTime sql=r_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show

ScheduleItem sql=rozpis_item
  Id sql=ri_id
  parent ScheduleId sql=ri_id_rodic
  partner UserId sql=ri_partner default=0
  from Day sql=ri_od
  to Day sql=ri_do
  lock Bool sql=ri_lock default=0
  deriving Show

UserGroup sql=skupiny
  Id sql=s_id
  name Text sql=s_name sqltype=VARCHAR(50)
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
  lock Bool sql=up_lock default=0
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

User sql=users
  Id sql=u_id
  login Text sql=u_login sqltype=CHAR(20)
  password Text sql=u_pass sqltype=CHAR(40)
  name Text sql=u_jmeno sqltype=VARCHAR(40)
  surname Text sql=u_prijmeni sqltype=VARCHAR(40)
  gender Text sql=u_pohlavi
  email Text sql=u_email sqltype=VARCHAR(50)
  phone Text sql=u_telefon sqltype=CHAR(17)
  birthDate Day sql=u_narozeni
  notes Text sql=u_poznamky
  updatedAt UTCTime sql=u_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  level Int sql=u_level default=0 sqltype=TINYINT(3)
  paymentGroup PaymentGroupId sql=u_group
  userGroup UserGroupId sql=u_skupina default=1
  dancer Bool sql=u_dancer default=0
  ban Bool sql=u_ban default=0
  lock Bool sql=u_lock default=0
  confirmed Bool sql=u_confirmed default=0
  temporary Bool sql=u_temporary default=0
  system Bool sql=u_system default=0
  deriving Show

Video sql=video
  Id sql=v_id
  title Text sql=v_name sqltype=VARCHAR(255)
  createdAt Day sql=v_date
  uri Text sql=v_uri sqltype=VARCHAR(255)
  text Text sql=v_text MigrationOnly
  isPlaylist Bool sql=v_playlist
  updatedAt UTCTime sql=v_timestamp sqltype=TIMESTAMP default=CURRENT_TIMESTAMP
  deriving Show
|]
