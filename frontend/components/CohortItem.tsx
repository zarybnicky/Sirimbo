import * as React from 'react';
import { CohortExport } from 'components/CohortExport';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'react-feather';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';
import { CohortWithMembersFragment } from 'lib/graphql/Cohorts';
import { UserPublicFragment } from 'lib/graphql/User';
import { RichTextView } from 'components/RichTextView';
import { Dropdown } from './Dropdown';
import { usePermissions } from 'lib/data/use-permissions';

export function CohortItem({ item: item }: { item: CohortWithMembersFragment }) {
  const perms = usePermissions();

  return (
    <Card key={item.id} cohort={item} className="group break-inside-avoid">
      {perms.canEditCohort(item) && (
        <Dropdown
          className="absolute right-1 top-1"
          align="end"
          options={[{ title: 'Upravit', href: `/admin/skupiny/${item.id}` }]}
        />
      )}

      <div>
        {item.usersByUSkupina?.nodes?.length} členů
        <div className="text-lg">{item.sName}</div>
      </div>
      <RichTextView
        value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
      />

      <div className="flex flex-wrap gap-2 mt-3">
        <SimpleDialog
          title="Seznam členů"
          button={<button className="button button-red">Seznam členů</button>}
        >
          <div className="flex flex-col items-start">
            {item.usersByUSkupina?.nodes?.map((member) => (
              <UserDetailButton key={member.id} user={member} />
            ))}
          </div>
        </SimpleDialog>
        <CohortExport id={item.id} name={item.sName} />
      </div>
    </Card>
  );
}

const UserDetailButton: React.FC<{ user: UserPublicFragment }> = ({ user }) => {
  return (
    <SimpleDialog
      title={
        <div className="text-xl">
          {user.uJmeno} {user.uPrijmeni}
        </div>
      }
      button={
        <button className="underline text-stone-700">
          {user.uPrijmeni}, {user.uJmeno}
        </button>
      }
    >
      <ul className="flex flex-col gap-3 m-4">
        <li>
          <EmailIcon className="inline" /> {user.uEmail}
        </li>
        <li>
          <PhoneIcon className="inline" /> {user.uTelefon}
        </li>
      </ul>
    </SimpleDialog>
  );
};
