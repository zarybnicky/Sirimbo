import { useAuth } from '@app/ui/use-auth';
import { getAgeGroup } from '@app/ui/get-age-group';
import { CohortListDocument } from '@app/graphql/Cohorts';
import { MyLessonsDocument } from '@app/graphql/Schedule';
import React from 'react';
import { Edit } from 'lucide-react';
import { PersonalInfoForm } from '@app/ui/PersonalInfoForm';
import { ChangePasswordForm } from '@app/ui/ChangePasswordForm';
import { LessonButton } from '@app/ui/LessonButton';
import { PermissionKey, PermissionLevel } from '@app/ui/use-permissions';
import type { NextPageWithLayout } from 'pages/_app';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { useQuery } from 'urql';
import { TitleBar } from '@app/ui/TitleBar';
import { NextSeo } from 'next-seo';

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
      <NextSeo title="Profil" />
      <TitleBar title={`${user.uJmeno} ${user.uPrijmeni}`}>
        <Dialog open={editOpen} onOpenChange={setEditOpen}>
          <DialogTrigger asChild>
            <button className="button-nav"><Edit />Upravit osobní údaje</button>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Osobní údaje</DialogTitle>
            <PersonalInfoForm onSuccess={editClose} />
          </DialogContent>
        </Dialog>

        <Dialog open={passOpen} onOpenChange={setPassOpen}>
          <DialogTrigger asChild>
            <button className="button-nav">Změnit heslo</button>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Změnit heslo</DialogTitle>
            <ChangePasswordForm onSuccess={passClose} />
          </DialogContent>
        </Dialog>
      </TitleBar>

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
      <div className="space-y-[1px] w-80">
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

Page.permissions = [PermissionKey.peNastenka, PermissionLevel.P_VIEW];

export default Page;
