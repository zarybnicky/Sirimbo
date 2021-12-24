import { HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { gql } from 'graphql-tag';

export const createClient = () => new ApolloClient({
  link: new HttpLink({ uri: '/graphql' }),
  cache: new InMemoryCache(),
});

export const UserQuery = gql(`
query UserQuery {
  getCurrentUser {
    permissionByUGroup {
      peAkce
      peAnkety
      peAktuality
      peDescription
      peDokumenty
      peGalerie
      peId
      peKonzole
      peInzerce
      peNabidka
      peMain
      peName
      peNastenka
      peNovinky
      pePary
      pePermissions
      pePlatby
      peRozpis
      peSkupiny
      peUsers
    }
    uTimestamp
    uSystem
    uTelefon
    uTeacher
    uStreet
    uRodneCislo
    uSkupina
    uPrijmeni
    uPoznamky
    uPostalCode
    uPohlavi
    uPass
    uOrientationNumber
    uNationality
    uNarozeni
    uMemberUntil
    uLogin
    uMemberSince
    uLock
    uLevel
    uJmeno
    uGroup
    uId
    uGdprSignedAt
    uEmail
    uDancer
    uDistrict
    uCreatedAt
    uConfirmed
    uConscriptionNumber
    uBan
    uCity
  }
}`);
