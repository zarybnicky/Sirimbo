'use client';

import {
  CreateInvitationDocument,
  PeopleWithAnotherAccountDocument,
  PeopleWithInvitationDocument,
  PeopleWithoutInvitationDocument,
} from '@/graphql/Invitation';
import { UserListDocument } from '@/graphql/CurrentUser';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { useActionMap } from '@/lib/actions';
import { personInvitationActions } from '@/lib/actions/personInvitation';
import { ActionRow } from '@/ui/ActionRow';
import { fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { PageHeader } from '@/ui/TitleBar';
import Link from 'next/link';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';

export function Invitations() {
  const [{ data: withAnotherAccount }] = useQuery({
    query: PeopleWithAnotherAccountDocument,
  });
  const [{ data: withoutInvitation }] = useQuery({
    query: PeopleWithoutInvitationDocument,
  });
  const [{ data: withInvitation }] = useQuery({ query: PeopleWithInvitationDocument });
  const [{ data: userQuery }] = useQuery({ query: UserListDocument });
  const [, sendInvitation] = useMutation(CreateInvitationDocument);
  const [, createUserProxy] = useMutation(CreateUserProxyDocument);
  const invitations =
    withInvitation?.peopleWithoutAccessWithInvitationList?.flatMap(
      (person) => person.personInvitationsList,
    ) ?? [];
  const invitationActionMap = useActionMap(personInvitationActions, invitations);

  const usersByEmail = new Map<string, string[]>();
  for (const user of userQuery?.users?.nodes ?? []) {
    const email = user.uEmail.trim().toLowerCase();
    usersByEmail.set(email, [...(usersByEmail.get(email) ?? []), user.id]);
  }

  const bulkLinkAccounts = useAsyncCallback(async () => {
    for (const person of withAnotherAccount?.peopleWithoutAccessWithExistingAccountList ?? []) {
      if (!person.email) continue;
      const userIds = usersByEmail.get(person.email.trim().toLowerCase()) ?? [];
      if (userIds.length !== 1) continue;
      const result = await createUserProxy({
        input: { userProxy: { personId: person.id, userId: userIds[0]! } },
      });
      if (result.error) throw result.error;
    }
  });

  const bulkSendInvitations = useAsyncCallback(async () => {
    const sent = new Set<string>();
    for (const person of withoutInvitation?.peopleWithoutAccessOrInvitationList || []) {
      if (!person.email) continue;
      const email = person.email.trim().toLowerCase();
      if (sent.has(email)) continue;
      sent.add(email);
      const result = await sendInvitation({
        input: {
          personInvitation: {
            personId: person.id,
            email: person.email,
          },
        },
      });
      if (result.error) throw result.error;
    }
  });

  return (
    <>
      <PageHeader title="Přehled pozvánek" />

      <div className="prose prose-accent">
        {(withAnotherAccount?.peopleWithoutAccessWithExistingAccountList || []).length >
          0 && (
          <>
            <h2>
              Osoby bez přístupu do systému - s jiným existujícím účtem podle e-mailu,
              stačí přiřadit
            </h2>
            <SubmitButton type="button" action={bulkLinkAccounts}>
              Přiřadit jednoznačné účty
            </SubmitButton>
            <ul>
              {withAnotherAccount?.peopleWithoutAccessWithExistingAccountList?.map(
                (x) => (
                  <li key={x.id}>
                    <Link href={`/clenove/${x.id}`}>{x.name}</Link> ({x.email})
                    {(usersByEmail.get(x.email?.trim().toLowerCase() ?? '') ?? []).length >
                      1 && ' - více uživatelských účtů se stejným e-mailem'}
                  </li>
                ),
              )}
            </ul>
          </>
        )}

        {(
          withoutInvitation?.peopleWithoutAccessOrInvitationList?.filter(
            (x) => !x.email,
          ) || []
        ).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní, bez e-mailu</h2>
            <ul>
              {withoutInvitation?.peopleWithoutAccessOrInvitationList
                ?.filter((x) => !x.email)
                .map((x) => (
                  <li key={x.id}>
                    <Link href={`/clenove/${x.id}`}>{x.name}</Link>
                    {', vytvořen '}
                    {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                    {x.email ? '' : <b>, chybí e-mail, není kam poslat pozvánku</b>}
                  </li>
                ))}
            </ul>
          </>
        )}

        {(
          withoutInvitation?.peopleWithoutAccessOrInvitationList?.filter(
            (x) => x.email,
          ) || []
        ).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní</h2>

            <SubmitButton type="button" action={bulkSendInvitations}>
              Pozvat všechny
            </SubmitButton>

            <ul>
              {withoutInvitation?.peopleWithoutAccessOrInvitationList
                ?.filter((x) => x.email)
                .map((x) => (
                  <li key={x.id}>
                    <Link href={`/clenove/${x.id}`}>{x.name}</Link>
                    {', vytvořen '}
                    {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                  </li>
                ))}
            </ul>
          </>
        )}

        {(withInvitation?.peopleWithoutAccessWithInvitationList || []).length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - již pozvaní</h2>
            <ul>
              {withInvitation?.peopleWithoutAccessWithInvitationList
                ?.toSorted((x, y) => x.createdAt.localeCompare(y.createdAt))
                .map((x) => (
                  <li key={x.id}>
                    <div>
                      <Link href={`/clenove/${x.id}`}>{x.name}</Link>
                      {', vytvořen '}
                      {x.createdAt ? fullDateFormatter.format(new Date(x.createdAt)) : ''}
                    </div>
                    <div className="not-prose mt-2 space-y-1">
                      {x.personInvitationsList.map((invitation) => (
                        <ActionRow
                          key={invitation.id}
                          actions={invitationActionMap.get(invitation.id)!}
                          className="mb-0 text-sm"
                        >
                          <span>
                            Pozvánka odeslána{' '}
                            {fullDateFormatter.format(new Date(invitation.createdAt))}
                          </span>
                        </ActionRow>
                      ))}
                    </div>
                  </li>
                ))}
            </ul>
          </>
        )}

        {(withAnotherAccount?.peopleWithoutAccessWithExistingAccountList || []).length ===
          0 &&
          (withoutInvitation?.peopleWithoutAccessOrInvitationList || []).length === 0 &&
          (withInvitation?.peopleWithoutAccessWithInvitationList || []).length === 0 && (
            <div>✅ Všechny v pořádku</div>
          )}
      </div>
    </>
  );
}
