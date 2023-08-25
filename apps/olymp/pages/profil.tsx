import { useAuth } from '@app/ui/use-auth';
import { getAgeGroup } from '@app/ui/get-age-group';
import { formatLongCoupleName } from '@app/ui/format';
import React from 'react';
import { EditPersonDialog } from '@app/ui/EditPersonDialog';
import { ChangePasswordDialog } from '@app/ui/ChangePasswordDialog';
import { TitleBar } from '@app/ui/TitleBar';
import { Layout } from 'components/layout/Layout';

const Page = () => {
  const { cohorts, couples, persons } = useAuth();

  return (
    <Layout requireMember>
      <TitleBar title="Můj profil">
        <ChangePasswordDialog />
      </TitleBar>

      {persons.map((x) => (
        <div key={x.id}>
          <EditPersonDialog id={x.id} />
          <p>
            {x.firstName} {x.lastName}
          </p>
          <p>
            Variabilní symbol:{' '}
            {(x.legacyUserId || x.nationalIdNumber || x.id).padStart(6, '0')}
          </p>

          <p>Věková kategorie: {x.birthDate ? getAgeGroup(new Date(x.birthDate).getFullYear()) : "(vyplňte datum narození)"}</p>
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
