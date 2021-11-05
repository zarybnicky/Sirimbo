import { gql } from 'graphql-tag';

export const galleryDirFields = gql`
fragment galleryDirFields on galerie_dir {
  gd_hidden
  gd_id
  gd_id_rodic
  gd_level
  gd_name
  gd_path
}`;

export const scheduleFields = gql`
fragment scheduleFields on rozpis {
  r_datum
  r_id
  r_kde
  r_lock
  r_timestamp
  r_trener
  r_visible
  user {
    u_jmeno
    u_prijmeni
    u_id
  }
}`;

export const scheduleItemFields = gql`
fragment scheduleItemFields on rozpis {
  rozpis_items {
    ri_od
    ri_do
    ri_id
    ri_partner
  }
}`;

export const reservationFields = gql`
fragment reservationFields on nabidka {
  n_visible
  n_trener
  n_timestamp
  n_pocet_hod
  n_od
  n_max_pocet_hod
  n_lock
  n_id
  n_do
  user {
    u_jmeno
    u_prijmeni
    u_id
  }
}`;

export const reservationItemFields = gql`
fragment reservationItemFields on nabidka {
  nabidka_items {
    ni_lock
    ni_partner
    ni_pocet_hod
    pary {
      user {
        u_id
        u_jmeno
        u_prijmeni
      }
    }
  }
}`;

export const eventFields = gql`
fragment eventFields on akce {
  a_do
  a_id
  a_info
  a_dokumenty
  a_jmeno
  a_kapacita
  a_kde
  a_lock
  a_od
  a_timestamp
  a_visible
}`;

export const eventItemFields = gql`
fragment eventItemFields on akce {
  akce_items {
    ai_id
    user {
      u_jmeno
      u_prijmeni
      u_id
    }
  }
}`;
