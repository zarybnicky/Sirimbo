import { useAuth } from '@app/ui/use-auth';
import { getAgeGroup } from '@app/ui/get-age-group';
import { formatLongCoupleName } from '@app/ui/format-name';
import React from 'react';
import { Edit } from 'lucide-react';
import { PersonForm } from '@app/ui/PersonForm';
import { ChangePasswordDialog } from '@app/ui/ChangePasswordDialog';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from 'components/layout/Layout';
import { buttonCls } from '@app/ui/style/button';

const Page = () => {
  const { cohorts, couples, persons } = useAuth();

  const [editOpen, setEditOpen] = React.useState(false);
  const editClose = React.useCallback(() => setEditOpen(false), []);

  return (
    <Layout requireMember>
      <TitleBar title="Můj profil">
        <ChangePasswordDialog />
      </TitleBar>

      {persons.map((x) => (
        <div key={x.id}>
          <Dialog open={editOpen} onOpenChange={setEditOpen}>
            <DialogTrigger asChild>
              <button className={buttonCls({ size: 'sm', variant: 'outline' })}>
                <Edit />
                Upravit osobní údaje
              </button>
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Osobní údaje</DialogTitle>
              <PersonForm id={x.id} onSuccess={editClose} />
            </DialogContent>
          </Dialog>
          <p>
            {x.firstName} {x.lastName}
          </p>
          <p>
            Variabilní symbol:{' '}
            {(x.legacyUserId || x.nationalIdNumber || x.id).padStart(6, '0')}
          </p>

          <p>Věková kategorie: {getAgeGroup(new Date(x.birthDate).getFullYear())}</p>
        </div>
      ))}

      <p>Tréninkové skupiny: {cohorts.map((x) => x.sName).join(', ')}</p>

      {couples.map((x) => (
        <p key={x.id}>Aktuální pár: {formatLongCoupleName(x)}</p>
      ))}
    </Layout>
  );
};

export default Page;
