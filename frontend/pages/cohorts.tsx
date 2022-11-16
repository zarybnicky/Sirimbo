import * as React from 'react';
import { useRequireUserLoggedIn } from 'lib/route-guards';
import { useMemberListQuery } from 'lib/graphql';
import { CohortExport } from 'components/CohortExport';
import { TabMenu } from 'components/TabMenu';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'react-feather';
import { UserFragment } from 'lib/graphql';
import { HtmlView } from 'components/HtmlView';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';

export default function CohortsPage() {
  useRequireUserLoggedIn();
  const { data: members } = useMemberListQuery();
  const cohorts = React.useMemo(() => {
    const cohorts: {
      [sId: string]: { sId: string; sDescription: string; sColorRgb: string; sName: string; members: any[] }
    } = {};
    members?.members?.nodes?.forEach(member => {
      if (!member.sVisible) return;
      if (!cohorts[member.sId!]) {
        cohorts[member.sId!] = {
          sId: member.sId || '',
          sName: member.sName || '',
          sColorRgb: member.sColorRgb || '',
          sDescription: member.sDescription || '',
          members: [],
        };
      }
      cohorts[member.sId!]!.members.push(member);
    });
    Object.values(cohorts).forEach(cohort => {
      cohort.members.sort((x, y) => `${x.uPrijmeni} ${x.uJmeno}`.localeCompare(`${y.uPrijmeni} ${y.uJmeno}`));
    });
    return cohorts;
  }, [members]);
  const [variant, setVariant] = React.useState('all');

  return <div className="container mx-auto max-w-5xl" style={{ margin: '2rem auto 6rem' }}>
    <div className="flex items-center justify-between gap-2 pb-2">
      <TabMenu selected={variant} onSelect={setVariant} options={[
        { id: 'cohortsOnly', label: 'Tréninkové skupiny' },
        { id: 'all', label: 'Členové dle skupin' },
      ]} />
      <CohortExport />
    </div>

    <div className="gap-4 lg:columns-2">
      {Object.values(cohorts).map(cohort => (
        <Card key={cohort.sId} className="break-inside-avoid mb-4 p-8">
          <div className="flex items-start justify-between mb-3">
            <div>
              {cohort.members.length} členů
              <h5>{cohort.sName}</h5>
            </div>
            <CohortExport id={cohort.sId} name={cohort.sName} />
          </div>

          {(variant === 'cohortsOnly') ? (
            <HtmlView content={cohort.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')} />
          ) : (
            <div className="flex flex-col items-start">
              {cohort.members.map((member) => <UserDetailButton key={member.uId} user={member} />)}
            </div>
          )}
        </Card>
      ))}
    </div>
  </div>;
}

const UserDetailButton: React.FC<{ user: UserFragment }> = ({ user }) => {
  return <SimpleDialog
    title={`${user.uJmeno} ${user.uPrijmeni}`}
    button={<button className="underline text-slate-700">{user.uPrijmeni}, {user.uJmeno}</button>}
  >
    <ul className="flex flex-col gap-3 m-4 mt-0">
      <li><EmailIcon /> {user.uEmail}</li>
      <li><PhoneIcon /> {user.uTelefon}</li>
    </ul>
  </SimpleDialog>
}
