import { useAuth } from '@app/ui/use-auth';
import { getAgeGroup } from '@app/ui/get-age-group';
import React from 'react';
import { Edit } from 'lucide-react';
import { PersonalInfoForm } from '@app/ui/PersonalInfoForm';
import { ChangePasswordForm } from '@app/ui/ChangePasswordForm';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { TitleBar } from '@app/ui/TitleBar';
import { NextSeo } from 'next-seo';
import { Layout } from 'components/layout/Layout';
import { buttonCls } from '@app/ui/style/button';

const Page = () => {
  const { cohorts, couples, persons } = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);
  const editClose = React.useCallback(() => setEditOpen(false), []);
  const [passOpen, setPassOpen] = React.useState(false);
  const passClose = React.useCallback(() => setPassOpen(false), []);

  return (
    <Layout requireMember>
      <NextSeo title="Profil" />
      <TitleBar title="M-j profil">
        <Dialog open={editOpen} onOpenChange={setEditOpen}>
          <DialogTrigger asChild>
            <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
              <Edit />
              Upravit osobní údaje
            </button>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Osobní údaje</DialogTitle>
            <PersonalInfoForm onSuccess={editClose} />
          </DialogContent>
        </Dialog>

        <Dialog open={passOpen} onOpenChange={setPassOpen}>
          <DialogTrigger asChild>
            <button className={buttonCls({ size: 'sm', variant: 'outline' })}>Změnit heslo</button>
          </DialogTrigger>
          <DialogContent>
            <DialogTitle>Změnit heslo</DialogTitle>
            <ChangePasswordForm onSuccess={passClose} />
          </DialogContent>
        </Dialog>
      </TitleBar>

      {persons.map(x => (
        <>
          <p>Variabilní symbol: {(x.legacyUserId || x.nationalIdNumber || x.id).padStart(6, '0')}</p>

          <p>Věková kategorie: {getAgeGroup(new Date(x.birthDate).getFullYear())}</p>
        </>
      ))}

      <p>
        Tréninkové skupiny: {cohorts.map(x => x.sName).join(', ')}
      </p>

      {couples.map(x => (
        <p key={x.id}>
          Aktuální pár: {x.man?.firstName} {x.man?.lastName} - {x.woman?.firstName} {x.woman?.lastName}
        </p>
      ))}
    </Layout>
  );
};


export default Page;
