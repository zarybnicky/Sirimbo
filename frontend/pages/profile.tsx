import { Item } from 'components/layout/Item';
import { List } from 'components/layout/List';
import { useAuth } from 'lib/data/use-auth';
import { getAgeGroup } from 'lib/get-age-group';
import { CohortListDocument } from 'lib/graphql/Cohorts';
import { MyLessonsDocument } from 'lib/graphql/Schedule';
import React from 'react';
import { Edit } from 'lucide-react';
import { PersonalInfoForm } from 'components/PersonalInfoForm';
import { ChangePasswordForm } from 'components/ChangePasswordForm';
import { LessonButton } from 'components/LessonButton';
import { PermissionKey, PermissionLevel } from 'lib/data/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from 'components/ui/dialog';
import { useQuery } from 'urql';

const Page: NextPageWithLayout = () => {
  const { user, couple } = useAuth();
  const [{ data: cohorts }] = useQuery({query: CohortListDocument});

  const [{ data: pastLessons }] = useQuery({
    query: MyLessonsDocument,
    variables: {
      startDate: new Date(23, 1).toISOString().substring(0, 10),
      endDate: new Date().toISOString().substring(0, 10),
    },
  });
  const [{ data: upcomingLessons }] = useQuery({
    query: MyLessonsDocument,
    variables: {
      startDate: new Date().toISOString().substring(0, 10),
      endDate: new Date(2123, 1).toISOString().substring(0, 10),
    },
  });
  const [editOpen, setEditOpen] = React.useState(false);
  const editClose = React.useCallback(() => setEditOpen(false), []);
  const [passOpen, setPassOpen] = React.useState(false);
  const passClose = React.useCallback(() => setPassOpen(false), []);

  if (!user) return null;

  return (
    <div className="container p-4 lg:py-8">
      <Item.Titlebar title={`${user.uJmeno} ${user.uPrijmeni}`}>
        <Dialog open={editOpen} onOpenChange={setEditOpen}>
          <DialogTrigger asChild>
            <List.TitleButton icon={Edit}>Upravit osobní údaje</List.TitleButton>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Osobní údaje</DialogTitle>
            <PersonalInfoForm onSuccess={editClose} />
          </DialogContent>
        </Dialog>

        <Dialog open={passOpen} onOpenChange={setPassOpen}>
          <DialogTrigger asChild>
            <List.TitleButton icon={Edit}>Upravit heslo</List.TitleButton>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Změnit heslo</DialogTitle>
            <ChangePasswordForm onSuccess={passClose} />
          </DialogContent>
        </Dialog>
      </Item.Titlebar>

      <p>Variabilní symbol: {user.id.padStart(6, '0')}</p>

      <p>Věková kategorie: {getAgeGroup(new Date(user?.uNarozeni).getFullYear())}</p>

      <p>
        Tréninková skupina:{' '}
        {cohorts?.skupinies?.nodes.find((x) => x.id === user.uSkupina)?.sName}
      </p>

      {(couple?.userByPIdPartner || couple?.userByPIdPartnerka) && (
        <p>
          {couple?.pIdPartner === user.id
          ? (couple?.userByPIdPartnerka && `Aktuální partnerka: ${couple?.userByPIdPartnerka?.uJmeno} ${couple?.userByPIdPartnerka?.uPrijmeni}`)
          : (couple?.userByPIdPartner && `Aktuální partner: ${couple?.userByPIdPartner?.uJmeno} ${couple?.userByPIdPartner?.uPrijmeni}`)}
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
    </div>
  );
};

Page.staticTitle = 'Profil';
Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
