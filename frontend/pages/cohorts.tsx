import * as React from 'react';
import { CohortExport } from 'components/CohortExport';
import { useCohortListWithMembersQuery } from 'lib/graphql/Cohorts';
import {
  withServerPermissions,
  PermissionKey,
  PermissionLevel,
} from 'lib/data/use-server-permissions';
import { Item } from 'components/layout/Item';
import { CohortItem } from 'components/CohortItem';

export default function CohortsPage() {
  const { data: members } = useCohortListWithMembersQuery({ visible: true });

  return (
    <Item className="col-full-width">
      <Item.Titlebar title="Tréninkové skupiny">
        <CohortExport />
      </Item.Titlebar>

      <div className="gap-4 lg:columns-2 xl:columns-3">
        {members?.skupinies?.nodes.map((item) => (
          <CohortItem key={item.id} item={item} />
        ))}
      </div>
    </Item>
  );
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka,
  PermissionLevel.P_VIEW,
);
