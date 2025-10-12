import { gql } from 'urql';

export const LOGIN_MUTATION = gql`
  mutation MobileLogin($login: String!, $passwd: String!) {
    login(input: { login: $login, passwd: $passwd }) {
      result {
        jwt
        usr {
          id
          uLogin
          uEmail
          userProxiesList {
            id
            person {
              id
              name
              firstName
              lastName
              isTrainer
              isAdmin
              isMember
              allCouplesList {
                id
                name
              }
            }
          }
        }
      }
    }
  }
`;

export const CURRENT_USER_QUERY = gql`
  query MobileCurrentUser($versionId: String) {
    refreshJwt
    getCurrentUser(versionId: $versionId) {
      id
      uLogin
      uEmail
      userProxiesList {
        id
        person {
          id
          name
          firstName
          lastName
          isTrainer
          isAdmin
          isMember
          allCouplesList {
            id
            name
          }
        }
      }
    }
  }
`;

export const TRAINER_SCHEDULE_QUERY = gql`
  query TrainerSchedule($start: Datetime!, $end: Datetime, $trainerIds: [BigInt!]) {
    list: eventInstancesForRangeList(
      startRange: $start
      endRange: $end
      trainerIds: $trainerIds
    ) {
      id
      since
      until
      isCancelled
      event {
        id
        name
        summary
        locationText
      }
      trainers: eventInstanceTrainersByInstanceIdList {
        id
        name
        personId
      }
    }
  }
`;

export const PERSON_ATTENDANCE_QUERY = gql`
  query PersonAttendance($id: BigInt!) {
    person(id: $id) {
      id
      name
      recentAttendanceList {
        id
        status
        instance {
          id
          since
          until
          isCancelled
          event {
            id
            name
            summary
            locationText
          }
          trainers: eventInstanceTrainersByInstanceIdList {
            id
            name
          }
        }
      }
    }
  }
`;

export interface PersonSummary {
  id: string;
  name: string;
  firstName: string | null;
  lastName: string | null;
  isTrainer: boolean;
  isAdmin: boolean;
  isMember: boolean;
  allCouplesList: Array<{
    id: string;
    name: string;
  }>;
}

export interface UserProxy {
  id: string;
  person: PersonSummary | null;
}

export interface UserAuth {
  id: string;
  uLogin: string | null;
  uEmail: string;
  userProxiesList: UserProxy[];
}

export interface LoginResponse {
  login: {
    result: {
      jwt: string;
      usr: UserAuth | null;
    } | null;
  } | null;
}

export interface CurrentUserResponse {
  refreshJwt: string | null;
  getCurrentUser: UserAuth | null;
}

export interface TrainerScheduleResponse {
  list: Array<{
    id: string;
    since: string;
    until: string;
    isCancelled: boolean;
    event: {
      id: string;
      name: string;
      summary: string | null;
      locationText: string | null;
    } | null;
    trainers: Array<{
      id: string;
      name: string;
      personId: string | null;
    }>;
  }>;
}

export interface PersonAttendanceResponse {
  person: {
    id: string;
    name: string;
    recentAttendanceList: Array<{
      id: string;
      status: string;
      instance: {
        id: string;
        since: string;
        until: string;
        isCancelled: boolean;
        event: {
          id: string;
          name: string;
          summary: string | null;
          locationText: string | null;
        } | null;
        trainers: Array<{
          id: string;
          name: string;
        }>;
      } | null;
    }>;
  } | null;
}
