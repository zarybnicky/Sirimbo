import * as React from 'react';
import { saveAs } from 'file-saver';
import { useAuth } from '@app/ui/use-auth';
import { fetchGql } from '@app/graphql/query';
import { buttonCls } from '@app/ui/style';
import { PersonListDocument } from '@app/graphql/Person';
import { tenantId } from '@app/tenant/config.js';

export function CohortExport({ id, name }: { id?: string; name?: string }) {
  const { perms } = useAuth();

  const saveData = React.useCallback(
    async (e?: React.MouseEvent) => {
      e?.preventDefault();

      const { Workbook } = await import('exceljs');
      const data = await fetchGql(PersonListDocument, { inTenants: [tenantId], inCohort: id });
      const workbook = new Workbook();
      const worksheet = workbook.addWorksheet(name || 'Sheet 1');

      worksheet.columns = [
        { header: 'Jméno', key: 'firstName' },
        { header: 'Přijmení', key: 'lastName' },
        { header: 'Rodné číslo', key: 'birthNumber' },
        { header: 'Telefon', key: 'phone' },
        { header: 'E-mail', key: 'email' },
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
          birthNumber: x.nationalIdNumber,
          phone: x.phone,
          email: x.email,
        }),
      );

      const buf = await workbook.xlsx.writeBuffer();
      saveAs(new Blob([buf]), `${name || 'Všechny skupiny'}.xlsx`);
    },
    [id, name],
  );

  if (!perms.isTrainerOrAdmin) {
    return null;
  }

  return <button type="button" className={buttonCls({ variant: 'outline' })} onClick={saveData}>
    Export {name ? 'členů' : 'všech'}
  </button>;
}