import { Card } from '@app/ui/Card';
import { CohortExport } from '@app/ui/CohortExport';
import { RichTextView } from '@app/ui/RichTextView';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@app/ui/dialog';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'lucide-react';
import Link from 'next/link';
import * as React from 'react';
import { buttonCls } from '@app/ui/style';
import { useQuery } from 'urql';
import { CohortWithMembersDocument } from '@app/graphql/Cohorts';
import { PersonFragment } from '@app/graphql/Person';

export function CohortItem({ id }: { id: string }) {
  const [{ data }] = useQuery({ query: CohortWithMembersDocument, variables: { id }, pause: !id });
  const item = data?.entity;
  const members = data?.entity?.cohortMembershipsByCohortIdList || [];

  if (!item) return null;

  return (
    <Card cohort={item} className="group break-inside-avoid">
      <div>
        {!!members.length && `${members.length} členů`}
        <h5 className="text-lg">
          <Link href={`/treninkove-skupiny/${id}`}>{item.sName}</Link>
        </h5>
        <h6 className="font-bold mb-2">{item.sLocation}</h6>
      </div>
      <RichTextView
        value={item.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')}
      />

      {!!members.length && (
        <div className="flex flex-wrap gap-2 mt-3">
          <Dialog>
            <DialogTrigger asChild>
              <button className={buttonCls()}>Seznam členů</button>
            </DialogTrigger>
            <DialogContent>
              <DialogTitle>Seznam členů</DialogTitle>
              <div className="flex flex-col items-start">
                {members?.map((member) => (
                  <PersonDetailButton key={member.id} person={member.person!} />
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

const PersonDetailButton: React.FC<{
  person: PersonFragment
}> = ({ person }) => {
  return (
    <Dialog>
      <DialogTrigger className="underline text-neutral-12">
        {person.lastName}, {person.firstName}
      </DialogTrigger>
      <DialogContent>
        <DialogTitle className="text-xl">
          {person.firstName} {person.lastName}
        </DialogTitle>
        <ul className="space-y-3 m-4">
          <li>
            <EmailIcon className="inline" /> {person.primaryEmail}
          </li>
          <li>
            <PhoneIcon className="inline" /> {person.primaryPhone}
          </li>
        </ul>
      </DialogContent>
    </Dialog>
  );
};
