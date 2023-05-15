import { SimpleDialog } from 'components/Dialog';
import { Item } from 'components/layout/Item';
import { List } from 'components/layout/List';
import { useAuth } from 'lib/data/use-auth';
import { getAgeGroup } from 'lib/get-age-group';
import { CohortListDocument } from 'lib/graphql/Cohorts';
import { MyLessonsDocument } from 'lib/graphql/Schedule';
import React from 'react';
import { Edit } from 'react-feather';
import { PersonalInfoForm } from 'components/PersonalInfoForm';
import { ChangePasswordForm } from 'components/ChangePasswordForm';
import { LessonButton } from 'components/LessonButton';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import { type NextPageWithLayout } from 'pages/_app';
import { useGqlQuery } from 'lib/query';

const Page: NextPageWithLayout = () => {
  const { user, couple } = useAuth();
  const { data: cohorts } = useGqlQuery(CohortListDocument, {});

  const { data: pastLessons } = useGqlQuery(MyLessonsDocument, {
    startDate: new Date(23, 1).toISOString().substring(0, 10),
    endDate: new Date().toISOString().substring(0, 10),
  });
  const { data: upcomingLessons } = useGqlQuery(MyLessonsDocument, {
    startDate: new Date().toISOString().substring(0, 10),
    endDate: new Date(2123, 1).toISOString().substring(0, 10),
  });

  if (!user) return null;

  return (
    <Item className="col-full-width px-4 bg-stone-100">
      <Item.Titlebar title={`${user.uJmeno} ${user.uPrijmeni}`}>
        <SimpleDialog
          title="Osobní údaje"
          button={<List.TitleButton icon={Edit}>Upravit osobní údaje</List.TitleButton>}
        >
          {({ close }) => <PersonalInfoForm onSuccess={close} />}
        </SimpleDialog>

        <SimpleDialog
          title="Změnit heslo"
          button={<List.TitleButton icon={Edit}>Změnit heslo</List.TitleButton>}
        >
          {({ close }) => <ChangePasswordForm onSuccess={close} />}
        </SimpleDialog>
      </Item.Titlebar>

      <p>Variabilní symbol: {user.id.padStart(6, '0')}</p>

      <p>Věková kategorie: {getAgeGroup(new Date(user?.uNarozeni).getFullYear())}</p>

      <p>
        Tréninková skupina:{' '}
        {cohorts?.skupinies?.nodes.find((x) => x.id === user.uSkupina)?.sName}
      </p>

      {(couple?.userByPIdPartner || couple?.userByPIdPartnerka) && (
        <p>
          Aktuální partner:{' '}
          {couple?.pIdPartner === user.id
            ? `${couple?.userByPIdPartnerka?.uJmeno} ${couple?.userByPIdPartnerka?.uPrijmeni}`
            : `${couple?.userByPIdPartner?.uJmeno} ${couple?.userByPIdPartner?.uPrijmeni}`}
        </p>
      )}

      <h3>Nadcházející lekce</h3>
      <div className="flex flex-col gap-[1px] w-80">
        {upcomingLessons?.myLessons?.nodes.map((item) => (
          <LessonButton
            key={item.id}
            lesson={item}
            schedule={item.rozpiByRiIdRodic!}
            showTrainer
            showDate
          />
        ))}
      </div>

      <h3>Minulé lekce</h3>
      <div className="flex flex-col-reverse gap-[1px] w-80">
        {pastLessons?.myLessons?.nodes.map((item) => (
          <LessonButton
            key={item.id}
            lesson={item}
            schedule={item.rozpiByRiIdRodic!}
            showTrainer
            showDate
          />
        ))}
      </div>
    </Item>
  );
};

Page.staticTitle = 'Profil';
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
