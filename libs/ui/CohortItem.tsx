import * as React from 'react';
import { CohortExport } from '@app/ui/CohortExport';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'lucide-react';
import { Card, CardMenu } from '@app/ui/Card';
import { CohortWithMembersFragment } from '@app/graphql/Cohorts';
import { UserPublicFragment } from '@app/graphql/User';
import { RichTextView } from '@app/ui/RichTextView';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { useAuth } from './use-auth';
import { DropdownMenuLink } from './dropdown';

export function CohortItem({ item }: { item: CohortWithMembersFragment }) {
  const { perms } = useAuth();

  return (
    <Card cohort={item} className="group break-inside-avoid">
      {perms.canEditCohort(item) && (
        <CardMenu>
          <DropdownMenuLink href={{ pathname: '/admin/skupiny/[id]', query: { id: item.id } }}>
            Upravit
          </DropdownMenuLink>
        </CardMenu>
      )}

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
          <Dialog>
            <DialogTrigger asChild>
              <button className="button button-accent">Seznam členů</button>
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Seznam členů</DialogTitle>
              <div className="flex flex-col items-start">
                {item.usersByUSkupina?.nodes?.map((member) => (
                  <UserDetailButton key={member.id} user={member} />
                ))}
              </div>
            </DialogContent>
          </Dialog>
          <CohortExport id={item.id} name={item.sName} />
        </div>
      )}
    </Card>
  );
}

const UserDetailButton: React.FC<{ user: UserPublicFragment & { hasValidPayment: boolean | null } }> = ({ user }) => {
  return (
    <Dialog>
      <DialogTrigger className="underline text-neutral-12">
        {user.uPrijmeni}, {user.uJmeno}
      </DialogTrigger>
      <DialogContent>
        <DialogTitle className="text-xl">
          {user.uJmeno} {user.uPrijmeni}
        </DialogTitle>
        <ul className="space-y-3 m-4">
          <li>
            <EmailIcon className="inline" /> {user.uEmail}
          </li>
          <li>
            <PhoneIcon className="inline" /> {user.uTelefon}
          </li>
        </ul>
      </DialogContent>
    </Dialog>
  );
};
