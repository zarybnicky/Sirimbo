import { SimpleDialog } from "components/Dialog";
import { Item } from "components/layout/Item";
import { List } from "components/layout/List";
import { useAuth } from "lib/data/use-auth";
import { withServerPermissions, PermissionKey, PermissionLevel } from 'lib/data/use-server-permissions';
import { getAgeGroup } from 'lib/get-age-group';
import { useCohortListQuery } from "lib/graphql/Cohorts";
import React from "react";
import { Edit } from "react-feather";
import { PersonalInfoForm } from "components/PersonalInfoForm";
import { ChangePasswordForm } from "components/ChangePasswordForm";

export default function ProfilePage() {
  const { user, couple } = useAuth();
  const { data: cohorts } = useCohortListQuery();

  if (!user) return null;

  return <Item>
    <Item.Titlebar title={`${user.uJmeno} ${user.uPrijmeni}`}>
      <SimpleDialog
        title="Osobní údaje"
        button={<List.TitleButton icon={Edit}>Upravit osobní údaje</List.TitleButton>}
      >
        {({ close }) => <PersonalInfoForm onSuccess={close} />}
      </SimpleDialog>

      <SimpleDialog
        title="Změnit heslo"
        button={<List.TitleButton icon={Edit}>Změnit heslo</List.TitleButton>}
      >
        {({ close }) => <ChangePasswordForm onSuccess={close} />}
      </SimpleDialog>
    </Item.Titlebar>

    <p>Variabilní symbol: {user.id.padStart(6, '0')}</p>

    <p>Věková kategorie: {getAgeGroup(new Date(user?.uNarozeni).getFullYear())}</p>

    <p>Tréninková skupina: {cohorts?.skupinies?.nodes.find(x => x.id === user.uSkupina)?.sName}</p>

    <p>Aktuální partner: {couple?.pIdPartner === user.id ? (
      <>{couple?.userByPIdPartnerka?.uJmeno} {couple?.userByPIdPartnerka?.uPrijmeni}</>
    ) : (
      <>{couple?.userByPIdPartner?.uJmeno} {couple?.userByPIdPartner?.uPrijmeni}</>
    )}</p>
  </Item>
}

export const getServerSideProps = withServerPermissions(
  PermissionKey.peNastenka, PermissionLevel.P_VIEW,
);
