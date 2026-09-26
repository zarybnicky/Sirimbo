import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { useMutation } from 'urql';
import { FormError, useFormResult } from '@/ui/form.tsx';
import { TextFieldElement } from '@/ui/fields/text.tsx';
import { TextAreaElement } from '@/ui/fields/textarea.tsx';
import { SubmitButton } from '@/ui/submit.tsx';
import React from 'react';
import z from 'zod';
import {
  ReplaceTenantSettingsDocument,
  TenantFullFragment,
  UpdateTenantDocument,
} from '@/graphql/Tenant.ts';

const TenantFormSchema = z.object({
  name: z.string().min(1, 'Název je povinný'),
  description: z.string().optional(),
  bankAccount: z.string().optional(),
  origins: z.string().optional(),
  czIco: z.string().optional(),
  czDic: z.string().optional(),
  settings: z.string().refine((value) => {
    try {
      const parsed = JSON.parse(value);
      return parsed !== null && typeof parsed === 'object' && !Array.isArray(parsed);
    } catch {
      return false;
    }
  }, 'Nastavení musí být JSON objekt.'),
});

export function TenantEditForm({ tenant }: Readonly<{ tenant: TenantFullFragment }>) {
  const { control, handleSubmit } = useForm({
    resolver: zodResolver(TenantFormSchema),
    defaultValues: {
      name: tenant.name || '',
      description: tenant.description || '',
      bankAccount: tenant.bankAccount || '',
      origins: tenant.origins?.join(', ') ?? '',
      czIco: tenant.czIco || '',
      czDic: tenant.czDic || '',
      settings:
        typeof tenant.tenantSetting?.settings === 'string'
          ? tenant.tenantSetting.settings
          : JSON.stringify(tenant.tenantSetting?.settings ?? {}, null, 2),
    },
  });
  const [result, updateTenant] = useMutation(UpdateTenantDocument);
  const [settingsResult, replaceSettings] = useMutation(ReplaceTenantSettingsDocument);
  const { onSuccess } = useFormResult();

  const onSubmit = async (values: z.infer<typeof TenantFormSchema>) => {
    const updated = await updateTenant({
      input: {
        id: tenant.id,
        patch: {
          name: values.name,
          description: values.description ?? '',
          bankAccount: values.bankAccount ?? '',
          origins: (values.origins ?? '')
            .split(',')
            .map((value) => value.trim())
            .filter(Boolean),
          czIco: values.czIco ?? '',
          czDic: values.czDic ?? '',
        },
      },
    });

    if (updated.error) return;

    const saved = await replaceSettings({
      tenantId: tenant.id,
      settings: values.settings,
    });
    if (!saved.error) onSuccess();
  };

  return (
    <form className="grid gap-4" onSubmit={handleSubmit(onSubmit)}>
      <FormError error={result.error ?? settingsResult.error} />

      <TextFieldElement control={control} name="name" label="Název" required />
      <TextAreaElement
        control={control}
        name="description"
        label="Popis"
        className="min-h-24"
      />
      <TextFieldElement control={control} name="bankAccount" label="Bankovní účet" />
      <TextFieldElement
        control={control}
        name="origins"
        label="Domény (oddělené čárkou)"
        helperText="např. example.cz, www.example.cz"
      />
      <div className="grid gap-4 sm:grid-cols-2">
        <TextFieldElement control={control} name="czIco" label="IČO" />
        <TextFieldElement control={control} name="czDic" label="DIČ" />
      </div>
      <TextAreaElement
        control={control}
        name="settings"
        label="Nastavení (JSON)"
        className="font-mono"
      />

      <SubmitButton control={control}>Uložit změny</SubmitButton>
    </form>
  );
}
