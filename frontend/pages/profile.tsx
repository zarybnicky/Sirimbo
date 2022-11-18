import { SimpleDialog } from "components/Dialog";
import { Item } from "components/layout/Item";
import { List } from "components/layout/List";
import { useAuth } from "lib/data/use-auth";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { getAgeGroup } from 'lib/get-age-group';
import React from "react";
import { Edit } from "react-feather";

export default function ProfilePage() {
  const { user, couple } = useAuth();

  if (!user) return null;

  return <Item>
    <Item.Titlebar title={`${user.uJmeno} ${user.uPrijmeni}`}>
      <SimpleDialog
        title="Osobní údaje"
        button={<List.TitleButton icon={Edit}>Upravit osobní údaje</List.TitleButton>}
      >

      </SimpleDialog>
    </Item.Titlebar>

    Věková kategorie: {getAgeGroup(new Date(user?.uNarozeni).getFullYear())}


  </Item>
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
