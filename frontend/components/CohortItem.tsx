import * as React from 'react';
import { CohortExport } from 'components/CohortExport';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'react-feather';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';
import { CohortWithMembersFragment } from 'lib/graphql/Cohorts';
import { UserPublicFragment } from 'lib/graphql/User';
import { RichTextView } from 'components/RichTextView';
import { Cohort } from 'lib/entities';
import { useAuth } from 'lib/data/use-auth';

export function CohortItem({ item }: { item: CohortWithMembersFragment }) {
  const { perms } = useAuth();

  return (
    <Card menu={Cohort.useMenu(item)} cohort={item} className="group break-inside-avoid">
      <div>
        {!!item.usersByUSkupina.nodes.length && (
          <>{item.usersByUSkupina?.nodes?.length} členů</>
        )}
        <h5 className="text-lg">{item.sName}</h5>
        <h6 className="font-bold mb-2">{item.sLocation}</h6>
      </div>
      <RichTextView
        value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
      />

      {!!item.usersByUSkupina.nodes.length && (
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
      )}
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
