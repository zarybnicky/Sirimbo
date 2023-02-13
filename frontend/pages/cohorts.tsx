import * as React from 'react';
import { CohortExport } from 'components/CohortExport';
import { TabMenu } from 'components/TabMenu';
import { AtSign as EmailIcon, Phone as PhoneIcon } from 'react-feather';
import { useMemberListQuery } from 'lib/graphql/User';
import { Card } from 'components/Card';
import { SimpleDialog } from 'components/Dialog';
import { UserFragment } from 'lib/graphql/CurrentUser';
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { RichTextView } from 'components/RichTextView';

export default function CohortsPage() {
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

  return <Item className="col-feature">
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
              <div className="text-lg">{cohort.sName}</div>
            </div>
            <CohortExport id={cohort.sId} name={cohort.sName} />
          </div>

          {variant === 'cohortsOnly' ? (
            <RichTextView value={cohort.sDescription.replaceAll('&nbsp;', ' ').replaceAll('<br /> ', '')} />
          ) : (
            <div className="flex flex-col items-start">
              {cohort.members.map((member) => <UserDetailButton key={member.uId} user={member} />)}
            </div>
          )}
        </Card>
      ))}
    </div>
  </Item>;
}

const UserDetailButton: React.FC<{ user: UserFragment }> = ({ user }) => {
  return <SimpleDialog
    title={<div className="text-xl">{user.uJmeno} {user.uPrijmeni}</div>}
    button={<button className="underline text-stone-700">{user.uPrijmeni}, {user.uJmeno}</button>}
  >
    <ul className="flex flex-col gap-3 m-4">
      <li><EmailIcon className="inline" /> {user.uEmail}</li>
      <li><PhoneIcon className="inline" /> {user.uTelefon}</li>
    </ul>
  </SimpleDialog>
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
