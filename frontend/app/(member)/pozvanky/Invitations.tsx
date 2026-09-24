'use client';

import {
  CreateInvitationDocument,
  InvitationOverviewDocument,
} from '@/graphql/Invitation';
import { CreateUserProxyDocument } from '@/graphql/Memberships';
import { useActionMap } from '@/lib/actions';
import { personInvitationActions } from '@/lib/actions/personInvitation';
import { ActionRow } from '@/ui/ActionRow';
import { fullDateFormatter } from '@/ui/format';
import { SubmitButton } from '@/ui/submit';
import { PageHeader } from '@/ui/TitleBar';
import Link from 'next/link';
import * as React from 'react';
import { useAsyncCallback } from 'react-async-hook';
import { useMutation, useQuery } from 'urql';

export function Invitations() {
  const [{ data: overview }] = useQuery({ query: InvitationOverviewDocument });
  const [, sendInvitation] = useMutation(CreateInvitationDocument);
  const [, createUserProxy] = useMutation(CreateUserProxyDocument);
  const report = React.useMemo(() => {
    const usersByEmail = new Map<string, string[]>();
    for (const user of overview?.users?.nodes ?? []) {
      const email = user.uEmail.trim().toLowerCase();
      usersByEmail.set(email, [...(usersByEmail.get(email) ?? []), user.id]);
    }

    const withAnotherAccount = [];
    const withoutInvitation = [];
    const withInvitation = [];
    for (const person of overview?.people?.nodes ?? []) {
      if (person.userProxiesList.length > 0) continue;
      if (person.personInvitationsList.length > 0) {
        withInvitation.push(person);
      } else if (usersByEmail.has(person.email?.trim().toLowerCase() ?? '')) {
        withAnotherAccount.push(person);
      } else {
        withoutInvitation.push(person);
      }
    }

    return {
      usersByEmail,
      withAnotherAccount,
      withoutInvitation,
      withInvitation,
      invitations: withInvitation.flatMap((x) => x.personInvitationsList),
    };
  }, [overview?.people?.nodes, overview?.users?.nodes]);
  const invitationActionMap = useActionMap(personInvitationActions, report.invitations);

  const bulkLinkAccounts = useAsyncCallback(async () => {
    for (const person of report.withAnotherAccount) {
      if (!person.email) continue;
      const userIds = report.usersByEmail.get(person.email.trim().toLowerCase()) ?? [];
      if (userIds.length !== 1) continue;
      const result = await createUserProxy({
        input: { userProxy: { personId: person.id, userId: userIds[0]! } },
      });
      if (result.error) throw result.error;
    }
  });

  const bulkSendInvitations = useAsyncCallback(async () => {
    const sent = new Set<string>();
    for (const person of report.withoutInvitation) {
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
        {report.withAnotherAccount.length > 0 && (
          <>
            <h2>
              Osoby bez přístupu do systému - s jiným existujícím účtem podle e-mailu,
              stačí přiřadit
            </h2>
            <SubmitButton type="button" action={bulkLinkAccounts}>
              Přiřadit jednoznačné účty
            </SubmitButton>
            <ul>
              {report.withAnotherAccount.map((x) => (
                <li key={x.id}>
                  <Link href={`/clenove/${x.id}`}>{x.name}</Link> ({x.email})
                  {(report.usersByEmail.get(x.email?.trim().toLowerCase() ?? '') ?? [])
                    .length > 1 && ' - více uživatelských účtů se stejným e-mailem'}
                </li>
              ))}
            </ul>
          </>
        )}

        {report.withoutInvitation.some((x) => !x.email) && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní, bez e-mailu</h2>
            <ul>
              {report.withoutInvitation
                .filter((x) => !x.email)
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

        {report.withoutInvitation.some((x) => x.email) && (
          <>
            <h2>Osoby bez přístupu do systému - ještě nepozvaní</h2>

            <SubmitButton type="button" action={bulkSendInvitations}>
              Pozvat všechny
            </SubmitButton>

            <ul>
              {report.withoutInvitation
                .filter((x) => x.email)
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

        {report.withInvitation.length > 0 && (
          <>
            <h2>Osoby bez přístupu do systému - již pozvaní</h2>
            <ul>
              {report.withInvitation
                .toSorted((x, y) => x.createdAt.localeCompare(y.createdAt))
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

        {report.withAnotherAccount.length === 0 &&
          report.withoutInvitation.length === 0 &&
          report.withInvitation.length === 0 && <div>✅ Všechny v pořádku</div>}
      </div>
    </>
  );
}
