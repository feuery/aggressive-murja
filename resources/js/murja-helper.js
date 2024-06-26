var app = Elm.Main.init({
    node: document.getElementById("app")
});
app.ports.alert.subscribe( (prompt) => {
    window.alert(prompt);
});

app.ports.prompt.subscribe( (prompt) => {
    let value = window.prompt(prompt);
    app.ports.tags.send(value);
});

app.ports.reallySetupAce.subscribe( (content) => {
    let editor = ace.edit("editor-post-content");

    if(!editor) {
	alert("Didn't find ace");
	return;
    }

    editor.setKeyboardHandler("ace/keyboard/emacs");
    editor.session.setValue(content);
    editor.on('change', event => {
     	let value = editor.getSession().getValue();
	app.ports.aceStateUpdate.send(value);
    });
});

app.ports.addImgToAce.subscribe(img_id => {
    let editor = ace.edit("editor-post-content");

    if (editor) {
	editor.insert('<img src="/api/pictures/' + img_id +'" />');

    } else alert("Didn't find ace editor");
});

Object.defineProperty(HTMLElement.prototype, "dangerouslySetInnerHTML", {
    get () {
        return this.innerHTML
    },
    set (value) {
        this.innerHTML = value
    }
});

app.ports.showPreviousPostsModal.subscribe(_ => {
    document.getElementById('previouslyModal').showModal();
});

app.ports.showPreviousPostPreviewModal.subscribe(_ => {
    document.getElementById('previewPreviouslyModal').showModal();
});

app.ports.showModal.subscribe(id => {
    document.getElementById(id).showModal();
});

app.ports.closePreviousPostsModal.subscribe(_ => {
    document.querySelectorAll('dialog').forEach(dialog => {
	dialog.close();
    });
});

app.ports.createExcerpt.subscribe(([textarea_id, feed_id]) => {
    let txt = document.getElementById(textarea_id)
    
    if ( !txt ) {
	alert(`Didn't find textarea with id ${textarea_id}`);
	return;
    }

    let start = txt.selectionStart;
    let finish = txt.selectionEnd;
    
    let selectedText = txt.value.substring(start, finish);
    app.ports.excerptCreated.send([selectedText, feed_id]);
});

    
