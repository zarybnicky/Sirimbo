import { HttpLink, ApolloClient, InMemoryCache } from '@apollo/client';
import { Selector, InputType, GraphQLTypes } from './zeus';
import { gql } from '@apollo/client';

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
export type AppUser = InputType<GraphQLTypes["User"], typeof UserPartial>;

export const UserMock: AppUser = {
  permissionByUGroup: {
    peAkce: 1,
    peAnkety: 1,
    peAktuality: 1,
    peDescription: "Guest",
    peDokumenty: 1,
    peGalerie: 1,
    peId: 5,
    peKonzole: 1,
    peInzerce: 1,
    peNabidka: 1,
    peMain: 1,
    peName: "Guest",
    peNastenka: 1,
    peNovinky: 1,
    pePary: 1,
    pePermissions: 1,
    pePlatby: 1,
    peRozpis: 1,
    peSkupiny: 1,
    peUsers: 1,
  },
  uTimestamp: true,
  uSystem: false,
  uTelefon: "734408237",
  uTeacher: true,
  uStreet: "Street",
  uRodneCislo: "9999990000",
  uSkupina: 5,
  uPrijmeni: "Surname",
  uPoznamky: "...",
  uPostalCode: "77777",
  uPohlavi: "m",
  uOrientationNumber: "4",
  uNationality: "CZ",
  uNarozeni: "2010-02-01",
  uMemberUntil: null,
  uLogin: "guest",
  uMemberSince: "2010-02-01T12:12:12Z",
  uLock: true,
  uLevel: 5,
  uJmeno: "Guest",
  uGroup: "5",
  uId: "5",
  uGdprSignedAt: "2010-02-01T12:12:12Z",
  uEmail: "a@a.com",
  uDancer: true,
  uDistrict: "District",
  uCreatedAt: "2010-02-01T12:12:12Z",
  uConfirmed: true,
  uConscriptionNumber: "4",
  uBan: false,
  uCity: "City",
};

export const UserPartial = Selector("User")({
  permissionByUGroup: {
    peAkce: true,
    peAnkety: true,
    peAktuality: true,
    peDescription: true,
    peDokumenty: true,
    peGalerie: true,
    peId: true,
    peKonzole: true,
    peInzerce: true,
    peNabidka: true,
    peMain: true,
    peName: true,
    peNastenka: true,
    peNovinky: true,
    pePary: true,
    pePermissions: true,
    pePlatby: true,
    peRozpis: true,
    peSkupiny: true,
    peUsers: true,
  },
  uTimestamp: true,
  uSystem: true,
  uTelefon: true,
  uTeacher: true,
  uStreet: true,
  uRodneCislo: true,
  uSkupina: true,
  uPrijmeni: true,
  uPoznamky: true,
  uPostalCode: true,
  uPohlavi: true,
  uOrientationNumber: true,
  uNationality: true,
  uNarozeni: true,
  uMemberUntil: true,
  uLogin: true,
  uMemberSince: true,
  uLock: true,
  uLevel: true,
  uJmeno: true,
  uGroup: true,
  uId: true,
  uGdprSignedAt: true,
  uEmail: true,
  uDancer: true,
  uDistrict: true,
  uCreatedAt: true,
  uConfirmed: true,
  uConscriptionNumber: true,
  uBan: true,
  uCity: true,
});
