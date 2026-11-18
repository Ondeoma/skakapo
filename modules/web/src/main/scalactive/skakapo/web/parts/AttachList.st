<script lang="scala">
import skakapo.web.*
import skakapo.shared.*

// 添付するファイルの一覧（画像は縮小表示、それ以外は名前と大きさ）と「＋ ファイル」. compact なら小さく出す（会話の返事の欄）
case class Props(attach: Attach, compact: Boolean = false)

def atts(props: Props) = props.attach.atts
def addLabel(props: Props) = props.attach.uploading.mapC(n => if (n > 0) "上げています…" else "＋ ファイル")
</script>

<template>
  <div :class='if (props.compact) "atts compact" else "atts"'>
    <div v-for="a in atts(props)" :key="a.id.toString" :class='if (a.isImage) "att img" else "att file"' :title="a.name">
      <img v-if="a.isImage" :src="AttachmentUrl(a)" :alt="a.name">
      <template v-else><span class="ic">📄</span><span class="nm">{{ a.name }}</span><span class="sz">{{ AttachmentRule.size(a.size) }}</span></template>
      <button type="button" class="ghost small rm" title="外す" @click="props.attach.remove(a)">×</button>
    </div>
    <label class="att add">
      <input type="file" multiple @change="props.attach.pick(event)">
      <span>{{ addLabel(props) }}</span>
    </label>
  </div>
</template>

<style>
.atts { display: flex; flex-wrap: wrap; gap: .5rem; }
.att { position: relative; width: 96px; height: 96px; border: 1px solid var(--line); border-radius: 8px; overflow: hidden; background: var(--hover); }
.att img { width: 100%; height: 100%; object-fit: cover; display: block; }
.att.file { display: flex; flex-direction: column; align-items: center; justify-content: center; gap: .15rem; padding: .3rem; text-align: center; }
.att.file .ic { font-size: 1.5rem; line-height: 1; }
.att.file .nm { font-size: .75rem; width: 100%; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
.att.file .sz { font-size: .7rem; color: var(--muted); }
.att .rm { position: absolute; top: 2px; right: 2px; background: var(--surface); padding: 0 .4rem; border-radius: 999px; }
.att.add { display: flex; align-items: center; justify-content: center; cursor: pointer; color: var(--muted); border-style: dashed; font-size: .85rem; }
.att.add input { display: none; }
.atts.compact .att { width: 72px; height: 72px; }
</style>
