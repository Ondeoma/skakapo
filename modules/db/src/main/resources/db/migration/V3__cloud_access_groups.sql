-- クラウドの資格情報の「使える人」に、利用者の種類で選ぶ方法（groups）を足す.
-- access: admins（管理者の依頼だけ）・all（全員の依頼）・groups（access_groups の種類の利用者と、管理者の依頼）
alter table cloud_credential drop constraint cloud_credential_access_check;
alter table cloud_credential add constraint cloud_credential_access_check check (access in ('admins', 'all', 'groups'));
alter table cloud_credential add column access_groups uuid[] not null default '{}';
