import * as React from 'react';
import { saveAs } from 'file-saver';
import { useAuth } from '@/ui/use-auth';
import { fetchGql } from '@/graphql/query';
import { buttonCls } from '@/ui/style';
import { PersonListDocument } from '@/graphql/Person';
import { fullDateFormatter } from './format';

export function CohortExportButton({ ids, name }: { ids: string[]; name?: string }) {
  const auth = useAuth();

  const saveData = React.useCallback(
    async (e?: React.MouseEvent) => {
      e?.preventDefault();

      const { Workbook } = await import('exceljs');
      const data = await fetchGql(PersonListDocument, { inCohorts: ids });
      const workbook = new Workbook();
      const worksheet = workbook.addWorksheet(name || 'Sheet 1');

      worksheet.columns = [
        { header: 'Jméno', key: 'firstName' },
        { header: 'Přijmení', key: 'lastName' },
        { header: 'Rodné číslo', key: 'birthNumber' },
        { header: 'Datum narození', key: 'birthDate' },
        { header: 'Telefon', key: 'phone' },
        { header: 'E-mail', key: 'email' },
        { header: 'Skupiny', key: 'cohorts' },
      ];

      worksheet.getRow(1).font = { bold: true };
      worksheet.columns.forEach((column) => {
        column.width = (column?.header?.length || 0) + 10;
        column.alignment = { horizontal: 'center' };
      });

      data.filteredPeopleList?.forEach((x) =>
        worksheet.addRow({
          firstName: x.firstName,
          lastName: x.lastName,
          birthNumber: x.taxIdentificationNumber,
          birthDate: x.birthDate ? fullDateFormatter.format(new Date(x.birthDate)) : '',
          phone: x.phone,
          email: x.email,
          cohorts: x.cohortMembershipsList.map(x => x.cohort?.sName).join(', '),
        }),
      );

      const buf = await workbook.xlsx.writeBuffer();
      saveAs(new Blob([buf]), `${name || 'Všechny skupiny'}.xlsx`);
    },
    [ids, name],
  );

  if (!auth.isTrainerOrAdmin) {
    return null;
  }

  return <button type="button" className={buttonCls({ size: 'sm', variant: 'outline' })} onClick={saveData}>
    Export {name ? 'členů' : 'všech'}
  </button>;
}
