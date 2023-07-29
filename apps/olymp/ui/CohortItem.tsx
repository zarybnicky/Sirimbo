import { UserPublicFragment } from '@app/graphql/User';
import { Card } from '@app/ui/Card';
import { CohortExport } from '@app/ui/CohortExport';
import { RichTextView } from '@app/ui/RichTextView';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { buttonCls } from './style/button';
import { useQuery } from 'urql';
import { CohortWithMembersDocument } from '@app/graphql/Cohorts';

export function CohortItem({ id }: { id: string }) {
  const [{ data }] = useQuery({
    query: CohortWithMembersDocument,
    variables: { id },
  });
  const item = data?.entity;

  if (!item) return null;

  return (
    <Card cohort={item} className="group break-inside-avoid">
      <div>
        {!!item.users.nodes.length && `${item.users?.nodes?.length} členů`}
        <h5 className="text-lg">
          <Link href={`/skupiny/${id}`}>{item.sName}</Link>
        </h5>
        <h6 className="font-bold mb-2">{item.sLocation}</h6>
      </div>
      <RichTextView
        value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
      />

      {!!item.users.nodes.length && (
        <div className="flex flex-wrap gap-2 mt-3">
          <Dialog>
            <DialogTrigger asChild>
              <button className={buttonCls()}>Seznam členů</button>
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Seznam členů</DialogTitle>
              <div className="flex flex-col items-start">
                {item.users?.nodes?.map((member) => (
                  <UserDetailButton key={member.id} user={member} />
                ))}
              </div>
            </DialogContent>
          </Dialog>
          <CohortExport id={id} name={item.sName} />
        </div>
      )}
    </Card>
  );
}

const UserDetailButton: React.FC<{
  user: UserPublicFragment & { hasValidPayment: boolean | null };
}> = ({ user }) => {
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
